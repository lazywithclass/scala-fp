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

