package ltbs.uniform.datapipeline

import org.scalatest._
import scala.language.implicitConversions

class DataParserSpec extends FlatSpec with Matchers {

  implicit def autoTree[A](in: A): Tree[String,List[A]] = Tree(List(in))
  def single[A](in:A): Tree[String,List[A]] = Tree(List(in))
  def list[A](in: A*): Tree[String,List[A]] = Tree(in.toList)

  "Options" should "return None when outer is false" in {
    val input: Input = Tree(List(""), Map("outer" -> "false"))
    implicitly[DataParser[Option[String]]].bind(input) should be (Right(None))
  }

  it should "parse nested values" in {
    val optBoolean = implicitly[DataParser[Option[Option[Boolean]]]]
    val r = optBoolean.bind(
      Tree(
        List(""),
        Map(
          "outer" -> "true",
          "inner" -> Tree(List(""), Map("outer" -> "true", "inner" -> "true"))
        )))
    r should be (Right(Some(Some(true))))
  }

  it should "not care about validation when empty" in {
    val optBoolean = implicitly[DataParser[Option[Boolean]]]
    val result = optBoolean.bind(
      Tree(
        List(""),
        Map(
          "outer" -> "false",
          "inner" -> "I'm some dirty data"
        ))
    )
    result should be (Right(None))
  }

  "formToInput" should "capture single input correctly" in {
    val in: FormUrlEncoded = Map("sf1" -> Seq("sv1","sv2"))
    formToInput(in) should be (Tree(List.empty, Map("sf1" -> list("sv1", "sv2"))))
  }

  it should "capture multiple input correctly" in {
    val in: FormUrlEncoded = Map("mf1" -> Seq("mv1","mv2"), "mf2" -> Seq("mv3","mv4"))
    formToInput(in) should be (Tree(List.empty,
                                    Map("mf1" -> list("mv1", "mv2"),
                                        "mf2" -> list("mv3", "mv4"))))
  }

  it should "capture nested input correctly" in {
    val in: FormUrlEncoded = Map("nf1.nf2" -> Seq("nv1","nv2"))
    formToInput(in) should be (
      Tree(
        List.empty,
        Map("nf1" -> Tree(List.empty, Map("nf2" -> list("nv1", "nv2"))))
      )
    )
  }

  "InferParser" should "decode a simple case class example" in {
    import InferParser._

    case class Blah(a: Boolean, b: String)
    val in: FormUrlEncoded = Map("a" -> Seq("TRUE"), "b" -> Seq("blahdy"))
    val tree = formToInput(in)
    val parser = implicitly[DataParser[Blah]]
    val output = parser.bind(tree)
    output should be (Right(Blah(true, "blahdy")))
  }

  it should "decode with options" in {
    import InferParser._

    case class Blah(a: Boolean, b: Option[String])
    val tree: Input = formToInput(Map(
      "a" -> Seq("TRUE"),
      "b.outer" -> Seq("true"),
      "b.inner" -> Seq("nested")
    ))
    val parser = implicitly[DataParser[Blah]]
    val output = parser.bind(tree)
    output should be (Right(Blah(true, Some("nested"))))
  }

  it should "decode with nested classes" in {
    import InferParser._

    case class One(a: Boolean, b: String)
    case class Two(oneone: One, onetwo: Option[One], other: Option[String])
    val tree: Input = formToInput(Map(
                                    "oneone.a" -> Seq("TRUE"),
                                    "oneone.b" -> Seq("ONE"),
                                    "onetwo.outer" -> Seq("true"),
                                    "onetwo.inner.a" -> Seq("TRUE"),
                                    "onetwo.inner.b" -> Seq("TWO"),
                                    "other.outer" -> Seq("FALSE")
                                  ))
    val parser = implicitly[DataParser[Two]]
    val output = parser.bind(tree)
    output should be (Right(Two(
                              One(true, "ONE"),
                              Some(One(true, "TWO")),
                              None
                            )))
  }

  it should "provide errors at the correct point" in {
    import InferParser._

    case class One(a: Option[Boolean], b: String)
    val tree: Input = formToInput(
      Map(
        "a.outer" -> Seq("true"),
        "b" -> Seq("sometext")
      ))
    val parser = implicitly[DataParser[One]]
    val output = parser.bind(tree)

    output should be (
      Left(Tree[String,String]("",Map("a" -> Tree[String,String]("",Map("inner" -> Tree[String,String]("required",Map()))))))
    )
  }

  it should "aggregate errors" in {
    import InferParser._

    case class One(a: Boolean, b: Option[Boolean])
    val tree: Input = formToInput(
      Map(
        "a" -> Seq("possibly"),
        "b.inner" -> Seq("nah")
      ))
    val parser = implicitly[DataParser[One]]
    val output = parser.bind(tree)

    output should be (
      Left(Tree[String,String]("",
                               Map(
                                 "a" -> Tree[String,String]("badValue"),
                                 "b" -> Tree[String,String]("",Map("outer" -> Tree[String,String]("required",Map()))))
           ))
    )
  }

}
