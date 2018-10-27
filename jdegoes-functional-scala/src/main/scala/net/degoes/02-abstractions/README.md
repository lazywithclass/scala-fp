## Abstractions

"Abstraction" and "typeclass" are interchangeable in FP.

### Semigroup 

Append two `A`s into an `A`.

(this code exists in scalaz)

`|+|` is the semigroup append operator.

```scala
object algebra {
  trait Semigroup[A] {
    def append(l: A, r: => A): A

    def associativityLaw(assertEquals: (A, ,A) => Boolean)(a: A, b: A, c: A): Boolean =
      assertEquals(
        append(a, append(b, c)), 
        append(append(a, b), c))
  }
  // summoner
  object Semigroup {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A
  }
  implicit class SemigroupSyntax[A{(l: A) {
    def |+| (r: => A)(implicit A: Semigroup[A]): A = A.append(l, r)
  }
}
```

### Monoid

One type class can build on another class.

Is a type of typeclass that build on Semigroup.

```scala
trait Monoid[A] extends Semigroup [a] {
  def zero: A

  def leftIdentity(AssertEqual: (A, A) => Boolean)(a: A): Boolean = 
    assertEquals(append(zero, a), a)

  def rightIdentity(AssertEqual: (A, A) => Boolean)(a: A): Boolean = 
    assertEquals(append(a, zero), a)
}
object Monoid {
  def apply[A](implicit A: Monoid[A]): Monoid[A] = a
}
final def mzero[A: Monoid]: A = Monoid[A].zero
```

Not all things that are semigroups are monoids.

The more powerful a typeclass is, the few implementations you might have of that.

Reasons to use Semigroups / Monoids / and so on:

 * a lot have already been implented
 * type soundness
 * laws allow you to write principled code
 * composition

Semigroups and Monoids allow to combine things in a lawful fashion.

### Functors

`Functor[F[_]] F: * => *`

Apply -> Applicative -> Monad

A functor has a single operation called `map`:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]

fa: F[A]
g: A => B
f: B => C
id[Z]: Z => Z  // functor identity law

// functor laws
map(fa)(id) == fa // identity law
map(map(fa)(g))(f) == ma(fa)(f.compose(g)) // composition law
// instead of mapping in two steps you can map once using compose
```

For example, to get the number of digits of each element in `Numbers`:

```scala
val Numbers  = List(12, 123, 0, 123981)
val Expected = List( 2,   3, 1,      6)
val g : Int => String = (i: Int) => i.toString
val f : String => Int = (s: String) => s.length
Numbers.map(identity)    == Numbers
Numbers.map(g andThen f) == Numbers.map(g).map(f)
```

Functors compose with other functors.

Imagine we are building our own program, with just two instructions:

 * Halt
 * Return A

which could be modelled as `Option`, so each of those are

 * None
 * Some(a)

Imagine we are building yet another progrqm, with just two instructions:

 * Halt
 * Return A And Then Do The Rest

which could be modelled as `List`, so each of those are
 
 * Nil
 * Cons(a, tail)

Product of two functors is a functor.

### Natural transformation

It's a function between Functors.

There are functions that are seen in "normal" functional programming, 
like zip, here zip merges two functors together.

You can combine together programs, futures, ...

### Monads

Also allow to combine programs.

Way more powerful than Functors, this power though means that they could be
applied to fewer types.

```scala
trait Monad[F[_]] extends Applicative[F] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}
// while zip was
// def zip[A, B](fa: F[A])(fb:     F[B]): F[(A, B)]
```

```scala
case class Employee(manager: Option[Person])
case class Person(address: Option[Address])
case class Address(postalCode: Option[Address])
val employee: Employee = ???
val postalCode: Option[Int] = 
  employee.manager.flatMap(person => 
    person.address.flatMap(address => 
      address.postalCode.map(postalCode => postalCode))
)

// or using list comprehension
val postalCode: Option[Int] = 
  for {
    person <- employee.manager
    address <- person.address
    postalCode <- address.postalCode
  } yield postalCode

```