# Scala with Cats

Working through the [Scala with Cats book](https://underscore.io/books/scala-with-cats/)

# Chapter 1 - Typeclasses
Used to add extra functions to an existing type

# Chapter 2 - Moniods and Semigroups

```scala
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

```

# Chapter 3 - Functors

```scala
import scala.language.higherKinds

//For appending an operation to a chain

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

//For prepending an operation to a chain.

trait ContraFunctor[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

//Combination of map and contramap.

trait IFunctor[F[_]] {
  def imap[A, B](fa: F[A])(dec: A => B)(enc: B => A): F[B]
}
```