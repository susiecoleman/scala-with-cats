import scala.language.higherKinds
import cats.Functor
import cats.syntax.functor._

object Chapter3 {

  def run = {
    import Printable2._
    import Codec._
//    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Leaf(100))
//    println(tree.map(_ * 2))
//    println(Tree.branch(Tree.leaf(4), Tree.leaf(4)).map(_ * 3))
//    println(Tree.leaf(4).map(_ * 4))

//    format("hello")
//    format(false)
//    println(format(Box("cat")))

//    println(encode(123.4))
//    println(decode[Double]("123.4"))
//    println(encode(Box(123.4)))
//    println(decode[Box[Double]]("123.4"))

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

trait Codec[A] {

  self =>

  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String = self.encode(enc(value))

    override def decode(value: String): B = dec(self.decode(value))
  }
}

object Codec {

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): String = value
  }

  implicit def doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  implicit def boxCodec[A, B](implicit codec: Codec[A]): Codec[Box[A]] = codec.imap[Box[A]](Box(_), _.value)

}

