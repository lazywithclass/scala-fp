## scala-fp

Following along John De Goes' course on [Functional Programming in Scala](https://gist.github.com/jdegoes/97459c0045f373f4eaf126998d8f65dc).

### Run

You will need docker, then run

```bash
$ make build
$ make
```

Then in the Docker vm run

```bash
$ ./sbt
sbt:Functional Scala> ~ compile
```

and code outside, code will be in sync.

### What I got from these three days

Composition. Type soundness.

It's impressive how things compose so nicely once you
lift the discussion into monoids.

### Dayly notes

[Day one](jdegoes-functional-scala/src/main/scala/net/degoes/01-essentials/)

[Day two](jdegoes-functional-scala/src/main/scala/net/degoes/02-abstractions/)

[Day three](jdegoes-functional-scala/src/main/scala/net/degoes/03-effects/)