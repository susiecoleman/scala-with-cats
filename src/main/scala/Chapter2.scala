import cats.Monoid
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._
import cats.instances.double._

// Monoids and Semigroups

object Chapter2 {
  def run = {

    Monoid[String].combine("Hello", " Cats")

    println(SuperAdder.add(List(1,2,3)))
    println(SuperAdder.add(List(Option(3), None, Option(1))))
    println(SuperAdder.add(List(Order(1.1,2), Order(4.5,6.7))))
  }

}

case class Order(totalCost: Double, quantity: Double)
object Order {
  implicit val addOrder: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0,0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}

object SuperAdder {

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.fold(monoid.empty)((x,y) => x |+| y)
  }
}

//trait Semigroup[A] {
//  def combine(x: A, y: A): A
//}
//
//trait Monoid[A] extends Semigroup[A] {
//  def empty: A
//}
//
//object Monoid {
//  def apply[A](implicit monoid: Monoid[A]) = monoid
//
//  implicit val orMonoid = new Monoid[Boolean] {
//    override def empty: Boolean = true
//
//    override def combine(x: Boolean, y: Boolean): Boolean = x || y
//  }
//
//  implicit val andMonoid = new Monoid[Boolean] {
//    override def empty: Boolean = true
//
//    override def combine(x: Boolean, y: Boolean): Boolean = x && y
//  }
//
//  implicit val addSets = new Monoid[Set[Int]] {
//    override def empty: Set[Int] = Set.empty[Int]
//
//    override def combine(x: Set[Int], y: Set[Int]): Set[Int] = x ++ y
//  }
//}
