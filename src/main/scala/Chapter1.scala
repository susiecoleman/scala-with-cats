object Chapter1 {
  import PrintableInstances._
  import PrintableSyntax._
  def run = {
//    Using the interface syntax
    Cat("oscar", 4).print
//    Using the interface Object
    Printable.print(123)
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

/*
NOTES
Implicits cannot be a the top level
implicitly function can be used to summon a value from the implicit scope.
 */