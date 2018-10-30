## Common problems in programmming solved in FP

Motivating

```scala
sealed trait Program[A]
object Program {
  case class Return[A](value: () => A) extends Program[A]
  case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
  case class ReadLine[A](next: String => Program[A]) extends Program[A]

  val unit: Program[Unit] = point(())

  // helper functions
  def writeLine(line: String): Program[Unit] = WriteLine(line, unit)
  def readLine: Program[String] = ReadLine(point(_))
  // we call it point just because return is a keyword in Scala
  def point[A](A: => A): Program[A] = Return(() => a)

  implicit val MonadProgram: Monad[Program] =
    new Monad(Program) {
      def point[A](a: => A): Program[A] = Program.point(a)
      // if we can write bind we get map and zip for free
      def bind[A, B](fa: Program[A])(f: A => Program[B]): Program[B] = 
        fa match {
          case Return(a) => f(a())
          case WriteLine(line, next) => WriteLine(line, bind(next)(f))
          case ReadLine(next) => ReadLine(input => bind(next(input))(f)
        }
    }
}

import Program.{readLine, printLine}

val helloWorld: Program[Unit] = 
  writeLine("Hello! What is your name?").flatMap(_ =>
    readLine.flatMap(name =>
      writeLine("Hello, " + name + "!")
      )
    )
  )

// same version using for comprehension
val helloWorld2: Program[Unit] = 
  for {
    _ <- writeLine("Hello! What is your name?")
    name <- readLine
    _ <- writeLine("Hello, " + name + "!")
  } yield ()
```

So we've build a model of our program (it doesn't actually do anything), then we 
will interpret this model with an effectful interpreter, external to our code so 
that we don't have side effects in our codebase.

Effect systems allow to bridge the gap between pure functional programming
and the real world, where you need to actually do stuff like 

 * calling a database
 * calling an API
 * etc

`IO[E, A]` - An immutable value that models an effectfull program that may fail 
with some error value of type E, run forever, or return some value fo type A.

Import foreign effect code easily into the purely functional world is of high importance,
zio in Scala makes this easy.

This is an example on how to do work with files

```scala
object classic {
  trait Handle
  def openFile(file: String): Handle = ???
  def closeFile(handle: Handle): Unit = ???
  def readFile(handle: Handle): Array[Byte] = ???
}
```

### Web crawler

If 

 * we can crawl one page
 * we can crawl another page
 * we can combine them

We're in business! This is a common way of organising things in combinatorics.

```scala
def crawlIO[E: Monoid, A: Monoid](
  seeds     : Set[URL],
  router    : URL => Set[URL],
  processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = ???
```

So the crawler can't fail, because of the `Nothing` in the `processor`, but 
it could fail while parsing the `String` representation of the page for 
example.

We `foldLeft` over the seeds and build the program

```scala
processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = 
  IO.traverse(seeds) { seed => 
    ( ??? : IO[Nothing, Crawl[E, A]])
  }.map(list => list.foldLeft(mzero[Crawl[E, A]])(_ |+| _))
  // or
//}.map(_.foldMap())
```

Folds over the elements using the zero of the Monoid as a starting point. And
remember that `E` and `A` are monoids.

`Crawl[E, A]` is a monoid.

Progressing with the implementation we get...

```scala
processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = 
  IO.traverse(seeds) { seed => 
    getURL(seed).redeem(
      // this is the empty crawl because of a bogus URL
      _ => IO.point(mzero[Crawl[E, A]]),
      html => 
        processor(url, html)
          .redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
    )
  }.map(list => list.foldLeft(mzero[Crawl[E, A]])(_ |+| _))
  // or
//}.map(_.foldMap())
```

We now can crawl the seeds, but we want to crawl the links in the seeds, 
so that our crawler can expand.

```scala
processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = 
  IO.traverse(seeds) { seed => 
    getURL(seed).redeem(
      // this is the empty crawl because of a bogus URL
      _ => IO.point(mzero[Crawl[E, A]]),
      html => 
        for {
          crawl <- processor(url, html)
            .redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
          urls = extractUrls(url, html).toSet.flatMap(router)
          crawl2 <- crawlIO(urls, router, processor)
        } yield crawl |+| crawl2
        // this is where we combine
    )
  }.map(_.foldMap())
```

Of course this will loop infinitely if urlA has a link to urlB which has a 
link to urlA, but it's a good point to stop and analyse the situation.

It's impressive how we can now just swap `traverse` for `parTraverse`,
and then have it all run in parallel.
