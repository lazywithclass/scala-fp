### Types

Types and functions are inseparable

A Set for example is

```scala
{ 1, 2, 3, "foo" }
```

order of elements is not important, each element is 
distinguishable from the others.

```scala
{ x | x is a human, x has no hair }

// ^ variables
        
    // ^ predicates that are true for those variables
```

```scala
// x ∈ A Int = 0 which is in Scala syntax:
val x : Int = 0
// this identifier is an element of the set identified on 
// the right hand side
```

Types and Sets are interchangeable. A type is a set of values.

Values exist in computer memory. Types do not exist in computer memory.

```scala
// compiles
def safeCast[A](n: Nothing): A = n
// does not compile
def notCast[A](u: Unit): A = u
```

Types compose with other types to give other types.

ADT are the foundation of data modelling in FP. We model business models with those.

```scala
type IPAddress = String
```

That is a sloppy representation of an IP address. I have to take care about `""` `"abc"`, etc.
Our `Set` is too big, there are lots of invalid state, so we have to deal with that in our code.

So let's do that.

```scala
final case class IPv4(_1: Byte, _2: Byte, _3: Byte, _4: Byte)
```

Product types and sum types

 * product of types `A` and `B` is another type such that `a` is an `A` and `b` is b `B`

```scala
A * B = { (a, b) | (a: A) AND (b: B) }

A = { true, false }
B = { "red", "green", "blue" }

// list all the elements in A * B
A * B = { {(true, "red"), (true, "green"), (true, "blue"), (false, "red"), (false, "green"), (false, "blue")} }

// the sum would've been
A + B = { true, false, "red", "green", "blue" }
```

Let's represent all elements in a Set as a line, which is a single dimensional space, and
`*` are elements

`-*---*---*---*--------------------`

If we add another Set `B`

```
A
|
|      *(b3, a4)
|
|
|
________________ B
```

To locate that point I need two values, this is the geometric interpretation of product composition.

```
// n-way product
A * B * C * ...

// the alphabet is a 26-way product, or "tuple-26"
```

Equivalent type -> you can write an invertible function that goes between them

`Nothing` is a subtype of every other type.

```scala
A * B = { (a, b) | (a : A) AND (b : B) }
A + B = { v      | (v : A) OR  (v : B) }
// or
A + B = { Left(v)  | v : A } UNION 
        { Right(v) | v : B }
```

This is the geometric representation of sum composition

```scala
A_a1_a2_a3_b1_b2_b3_ B
```

Canonical repesentation of sum type is `Either`, while `Tuple` is the one for product type.

```scala
A + A
A = { true, false }
A + A = { Left(true), Left(false), Right(true), Right(false) }
```

ADT are types composed from products and sums.

```scala
// sum type
// only allows certain colors
sealed trait Color
case object Red extends FixedColor
case object Blue extends FixedColor
case object Green extends FixedColor
// the Custom term happens to be a product type
case class Custom(red: Int, green: Int, blue: Int) extends Color

// product type
// allows more flexibility to list all colors I want
case class AnyColor(red: Int, green: Int, blue: Int)
```

ADT should be modelled as precisely enough. It's better not to have runtime erros

Sum type -> when types do not share the same properties.


### Functions

Mapping between two Sets, the domain A and the codomain B: `f: A => B`

Every single element in the domain, is associated with exactly one element in the codomain. 
Important to note that the codomain could be bigger than it needs to be, while all elements
in the domain are used, ie have a match in the codomain.

Guarantee that we know what `f(a)` if a is in the domain, and that answer is unambiguous.

Domain and codomains are types.

Functions properties:

 * totality: for every x in the domain, f(x) is in the codomain
 * determinism: if f(x) == f(y) whenever x == y
 * computable: the only effect of evaluating f(x) is computing the value (no side effects)

It's not necessary to do functional programming throughout the entire codebase!

```scala
def parseInt1(s: String): Int = s.toInt // breaks totality
```

Always defer devision to the upper levels of the applications, so lower pieces can remain
generic and then reusable.

```scala
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i)))
```

This is not a function because

 * it mutates the array
 * allows for `i` to be `0` which will then throw

Where it's easy to restrict the input always do that. With refinement types or the likes.

When we can delete an instruction and still have the function compile that's when we know 
we have a side effect.

```scala
// avoiding side effects
def buyCoffee1(processor: Processor, account: Account): Coffee = {
  val coffee = Coffee()
  // side effect!
  processor.charge(account, coffee.price)
  coffee
}
final case class Charge(account: Account, amount, Double)
def buyCoffee2(account: Account): (Coffee, Charge) =
  val coffee = new Coffee()
  // no side effect, capture the charge in a type!
  (coffee, Charge(account, coffee.price)
```

Just by looking at the types you can reason, this is a huge advantage of FP; names do not
matter, at least not as much as types!

Restrict the user of the function in such a way that they think "what's the only way I
can create this type?", so the implementation is trivial.

Effects can be thought of as ordinary values!

As per the Draw exercise, try to abstract away the state and have each function return a new version of it.
Another benefit of having a list of operations could open the way to optimisations, like for example if you're 
going right and then left without drawing you could eliminate that.

```scala
val f: Int => (String => String) = 
  i => s => if (i <= 1) s else s + f(i - 1)(s)
```

Codomain is the set of functions from String to String. 
A higher order function is a function whose Domain or codomain is itself a function.
Function combinators are functions that accept and return functions.
That above is a mono morphic function because it's dealing with String, Int; the opposite
is polymorphic functions, functions that have type parameters.

```scala
case class Box[A](value: A)
```

The above has a type parameter, `A`.

It could also become

```scala
def repeater[A](combine: (A, A) => A): Int => (A => A) = {
  val f0 = repeater(combine)
  i => s => if (i <= 1) s else combine(s, f0(i - 1)(s))
}

val stringRepeater = repeater[String](_ + _)
def listRepeater[A] = repeater[List[A]](_ ++ _)
```

So it could repeat anything. Type parameters allow for function polymorphism.

The FP game. It does not matter about what the function is doing, you could implement it step
by step because there's only one possible implementation.

 * first step: `def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = (a: A) => ( ??? : (B, C) )`
 * second step: `def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = (a: A) => ( (f(a), ???) : (B, C) )`
 * third step: `def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = (a: A) => ( (f(a), f(b) ) : (B, C) )`

In the end you end up implementing a function, correctly, without knowing what it does until you're done.

Tip: call variables by the name of their type.

Polymorphism

 * allows code reuse
 * cuts down to a single possible implementation

Monomorphic functions look more difficult to implement that polymorphic ones.

Types: a set of values.

`* = { x : x is a type }` // all x such that x is a type

`List` is not a type, it's a type constructor. You can't use it like a normal type: `val x: List = ???`

Type constructors are type level functions

 * `* => * = { Future, List, Option, Try, Vector, ... }`
 * `[*, *] => * = { Either, Map, Tuple2, IO }`

We say that `* => *` is the kind of `Future`, and that `[*, *] => *` is the kind of `Either`.

```scala
trait Foo[A[X]] {

  def foo[B[X]] = ???

}
```

`Foo` has type `* => *`.

`type FooList = Foo[List]`

```
trait Foo[A[_], B[_, _], C[_, _, _], D]
A : * => *
B : [*, *] => *
C : [*, *, *] => *
D : *

Foo : [(* => *) => *, ([*, *] => *), ([*, *, *] => *), *]
```

`* => *` is a higher kinded type.

Typeclass is a tuple of three things

 * a set of types and / or type constructors
 * a set of operations on values of those types
 * a set of laws governing the operations

A type class instance is an instance of a type class for a given 
set of types.

Talking about how to code a function in different ways than the one we 
did: "type here is so polymorphic that there's only one implementation".

### Recap

Type is a set of values; a value is something that sits
in memory.

`:` in a type declaration could be read as "element of".

Program -> collection of propositions, Compiler -> proof buddy.

ADT are types made of product types (and) or sum types (or).

`Tuple` product type, `Either` sum type.

We use ADTs to avoid illegal state and constraint number of 
implementations.

Monomorphic functions could be difficult to implement, 
despite the fact that they contain a single type.

A kind is a `Set` of types.

We use type classes, they allow code reuse, contraint possible implementations; they are a way to classify types.

Example of a type classs:

```scala
trait LessThan[A] {
  def lt(l: A, r: A): Boolean

  final def transitivityLaw(a: A, b: A, c: A): Boolean =
    lt(a, b) && lt(b, c) == lt(a, c) || (!lt(a, b) || !lt(b, c))
}
```
