package ltbs.uniform

import cats.implicits._
import play.api.i18n._
import collection.JavaConverters._

package object webmonad {

  implicit class RichMap[K,V](a: Map[K,V]) {

    /**  Union with a combining function */
    def unionWith(f: (V,V) => V, b: Map[K,V]): Map[K,V] = {
      a.map{ar =>
        b.get(ar._1).fold(ar)(bv => ar._1 -> f(ar._2,bv))
      } ++ b.filterNot{case (k,v) => a.isDefinedAt(k)}
    }
  }

  implicit class RichMessagesApi(inner: MessagesApi) {
    def add(
      newMessages: Map[String,Map[String,String]],
      collisionHandling: (String, String) => String = {case (a,_) => a}
    ): MessagesApi = new DefaultMessagesApi(
      inner.messages.unionWith(_.unionWith(collisionHandling,_), newMessages)
    )
  }

  implicit class RichMessages(mo: Messages.type) {

    def get(key: String, args: Any*)(implicit provider: Messages): Option[String] = {
      if (provider.isDefinedAt(key))
        provider.messages(key, args: _*).some
      else
        none[String]
    }

    def many(key: String, args: Any*)(implicit provider: Messages): List[String] = {

      @annotation.tailrec
      def inner(cnt: Int = 2, list: List[String] = Nil): List[String] =
        get(s"$key.$cnt", args: _*) match {
          case Some(_) => inner(cnt + 1, provider.messages(s"$key.$cnt", args: _*) :: list)
          case None => list
        }

      List(key, s"$key.1").flatMap(get(_, args)) ++ inner().reverse
    }

  }
}
