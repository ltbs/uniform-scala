package ltbs.uniform.datapipeline

import play.twirl.api.Html

trait Read[A] {
  def fromString(i: String): A
}

abstract class Messages[A: Read] {
  def apply(key: String, args: Any*): A = get(key, args:_*)
      .getOrElse(implicitly[Read[A]].fromString(key))

  def apply(key: List[String], args: Any*): A = get(key, args:_*)
      .getOrElse(implicitly[Read[A]].fromString(key.headOption.getOrElse("unknown")))

  def get(key: String, args: Any*): Option[A]
  def get(key: List[String], args: Any*): Option[A]
  def list(key: String, args: Any*): List[A]

  def decompose(key: String, args: Any*): A = decomposeOpt(key, args:_*)
      .getOrElse(implicitly[Read[A]].fromString(key))

  def decomposeOpt(key: String, args: Any*): Option[A] =
    get(Messages.decompositionList(key), args:_*)

  def keyValuePair(key: String, args: Any*): List[(A,A)]

  /** splitCamel is unsupported with JS - need to re-write with parser combinators */    
  // def decomposeOrBestGuess(key: String, args: Any*): A =
  //   decomposeOpt(key, args:_*) getOrElse {
  //     key.split("[.]").toList.reverse match {
  //       case "option"::x::_ => Messages.uncamel(x)
  //       case _ => key
  //     }
  //   }

  def map[B: Read](f: A => B): Messages[B] = {
    val orig = this
    new Messages[B] {
      def get(key: String, args: Any*): Option[B] = orig.get(key, args:_*).map(f)
      def get(key: List[String], args: Any*): Option[B] = orig.get(key, args:_*).map(f)
      def list(key: String, args: Any*): List[B] = orig.list(key, args:_*).map(f)
      def keyValuePair(key: String, args: Any*): List[(B,B)] = orig.keyValuePair(key, args:_*).map{
        case (k,v) => (f(k),f(v))
      }
    }
  }

}

object Messages {

  implicit val stringRead = new Read[String] {
    def fromString(in: String): String = in
  }

  implicit val htmlRead = new Read[Html] {
    def fromString(in: String): Html = Html(in)
  }

  def decompositionList(key: String): List[String] =
    key.split("[.]").toList.reverse match {
      case Nil | _::Nil | _::_::Nil => List(key)
      case t::hx =>
        hx.reverse.tails.toList.dropRight(1)
          .map{x => (x:+t).mkString(".")}
    }

  case class Noop[A: Read]() extends Messages[A] {
    def get(key: String, args: Any*): Option[A] = None
    def get(key: List[String], args: Any*): Option[A] = None    
    def list(key: String, args: Any*): List[A] = Nil
    def keyValuePair(key: String, args: Any*): List[(A,A)] = Nil
  }

  def fromPlayFormat(input: String): Messages[String] = {
    val rawLines = input
      .replace("\\\n"," ")
      .split("\n")      
      .map{_.replace("#.*","").split("=",2).toList}
      .collect {
        case (k::v::Nil) => (k, v.replace("''","'"))
      }

    val groupedAndSorted =
      rawLines.toList
        .groupBy(_._1.replaceAll("[.][0-9]*$",""))
        .mapValues{_.sortBy(_._1).map{_._2}}

    fromListMap(groupedAndSorted)
  }

  /** splitCamel is unsupported with JS - need to re-write with parser combinators */  
  def uncamel(in: String): String = splitCamel(in).toList match {
    case Nil => ""
    case x::xs => (x :: xs.map { w => w.toLowerCase match {
      case "and" | "or" | "to" | "from" => w.toLowerCase
      case _ => w
    }
    }).mkString(" ")
  }

  /** Unsupported with JS - need to re-write with parser combinators */
  def splitCamel(in: String): Iterable[String] =
    in.split("(?<!(^|[A-Z0-9]))(?=[A-Z0-9])|(?<!^)(?=[A-Z0-9][a-z])")

  def fromSimpleMap(
    underlying: Map[String, String]
  ): Messages[String] =
    fromListMap(underlying.mapValues(List(_)))

  def fromListMap(
    underlying: Map[String, List[String]]
  ): Messages[String] = new Messages[String] {

    @annotation.tailrec
    private def replaceArgs(
      input: String,
      args: List[String],
      count: Int = 0
    ): String = args match {
      case Nil    => input
      case h :: t => replaceArgs(input.replace(s"[{]$count[}]", h), t, count+1)
    }

    def get(key: List[String], args: Any*): Option[String] = {
      @annotation.tailrec
      def inner(innerkey: List[String]): Option[String] = {
        innerkey match {
          case Nil => None
          case x::xs =>
            get(x, args:_*) match {
              case None => inner(xs)
              case o    => o
            }
        }
      }
      inner(key)
    }
    
    def get(key: String, args: Any*): Option[String] =
      underlying.get(key).flatMap{_.headOption}.map{
        replaceArgs(_,args.toList.map(_.toString))
      }

    def list(key: String, args: Any*): List[String] =
      underlying.getOrElse(key,Nil).map{ x =>
        replaceArgs(x,args.toList.map(_.toString))
      }

    def keyValuePair(key: String, args: Any*): List[(String,String)] =
      list(key,args:_*) collect {
        case x if x.contains("|") =>
          val (k::v::Nil) = x.split("[|]").toList
          (k,v)
      }
  }
}
