import scala.language.higherKinds
import cats.Functor
import cats.syntax.functor._

object Chapter3 {

  def run = {
    import Printable2._
//    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Leaf(100))
//    println(tree.map(_ * 2))
//    println(Tree.branch(Tree.leaf(4), Tree.leaf(4)).map(_ * 3))
//    println(Tree.leaf(4).map(_ * 4))

//    format("hello")
//    format(false)
//    println(format(Box("cat")))

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

sealed trait Printable2[A] {

  self =>

  def format(x: A): String

  def contramap[B](func: B => A): Printable2[B] = {
    new Printable2[B] {
      override def format(value: B): String = self.format(func(value))
    }
  }
}

final case class Box[A](value: A)

object Printable2 {

  def format[A](value: A)(implicit p: Printable2[A]): String = p.format(value)

  implicit val stringPrintable: Printable2[String] = new Printable2[String] {
    override def format(value: String): String = value
  }

  implicit val booleanPrintable: Printable2[Boolean] = new Printable2[Boolean] {
    override def format(value: Boolean): String = if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit a: Printable2[A]): Printable2[Box[A]] =
    a.contramap[Box[A]](_.value)
}

