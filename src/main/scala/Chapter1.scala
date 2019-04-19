import cats.Show
import cats.Eq
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.option._
import cats.syntax.option._

// Typeclasses

object Chapter1 {
  import PrintableInstances._
  import PrintableSyntax._
  def run = {

    val oscar = Cat("oscar", 4)
    val mog = Cat("mog", 12)


//    Using the interface syntax
    oscar.print
//    Using the interface Object
    Printable.print(123)

    //Using Cats
    println(mog.show)
    println(mog === oscar)
    println(oscar === oscar)


    val optionalOscar = oscar.some

    println(optionalOscar =!= none[Cat])
  }
}

// Type Class
sealed trait Printable[A] {
  def format(x: A): String
}

// Type Class instances
object PrintableInstances {
  implicit val stringInstance: Printable[String] = new Printable[String] {
    def format(x: String): String = x
  }

  implicit val intInstance: Printable[Int] = new Printable[Int] {
    def format(x: Int): String = x.toString
  }

  implicit val catInstance: Printable[Cat] = new Printable[Cat] {
    def format(x: Cat): String = s"I am a cat called ${x.name}. I am ${x.age}."
  }
}

// Interface Object
object Printable {
  def format[A](x: A)(implicit printer: Printable[A]): String = {
    printer.format(x)
  }

  def print[A](x: A)(implicit printer: Printable[A]): Unit = {
    println(printer.format(x))
  }
}

object PrintableSyntax{
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = {
      printable.format(a)
    }

    def print(implicit printable: Printable[A]): Unit = {
      println(printable.format(a))
    }
  }
}

final case class Cat(name: String, age: Int)
object Cat {
  implicit val catShow: Show[Cat] = Show.show(cat => s"Hello, I'm ${cat.name} the cat. I'm ${cat.age} years old!")

  implicit val catEq: Eq[Cat] = Eq.instance[Cat]{(cat1, cat2) => cat1.name == cat2.name && cat1.age == cat2.age}
}


/*
NOTES
Implicits cannot be a the top level
implicitly function can be used to summon a value from the implicit scope.
 */