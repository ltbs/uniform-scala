package tree


import org.atnos.eff._
import org.atnos.eff.all.{none => _, _}

sealed trait UniformTree[S,A] {
  def key: String
}

case class UniformBranch[S,A](
  key: String,
  elements: Eff[S,A]
) extends UniformTree[S,A]

case class UniformLeaf[S,A](
  key: String
) extends UniformTree[S,A]



object TreeEffect {

  def leaf[S,A](key: String)(implicit member: UniformTree[A, ?] |= S): Eff[S, A] =
    send[UniformTree[A, ?], S, A](UniformLeaf(key))

  // def branch[B,S,A](key: String)(inner: Eff[B,A])(implicit member: UniformTree[A, ?] |= S, member2: Eff[B, ?] |= S): Eff[S, A] =
  //   send[UniformTree[B, ?], S, A](UniformBranch(key, inner))

}


object TreeTest {
  import TreeEffect._

  type _uniform[S,R] = UniformTree[S,?] |= R

  type TestProgramStack = Fx2[UniformTree[Int,?], UniformTree[String,?]]

  def program[R : _uniform[Int,?] : _uniform[String,?]]: Eff[R, Int] = for {
    one <- leaf[R,Int]("one")
//    two <- branch[R, R,Int]("branch")(leaf[R,Int]("two"))
  } yield (one)

}
