import scala.language.higherKinds
import cats.Functor
import cats.syntax.functor._

object Chapter3 {

  def run = {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Leaf(100))
    println(tree.map(_ * 2))
    println(Tree.branch(Leaf(4),Leaf(4)).map(_ * 3))
    println(Tree.leaf(4).map(_ * 4))

  }

  sealed trait Tree[+A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit  val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }

}
