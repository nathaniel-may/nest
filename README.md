# nest
[![CircleCI](https://circleci.com/gh/nathaniel-may/nest/tree/master.svg?style=svg)](https://circleci.com/gh/nathaniel-may/nest/tree/master)
[![codecov](https://codecov.io/gh/nathaniel-may/Nest/branch/master/graph/badge.svg)](https://codecov.io/gh/nathaniel-may/Nest)
[![Release](https://jitpack.io/v/nathaniel-may/nest.svg)](https://jitpack.io/#User/Repo)

A data type for managing nested pairs.  
  
Use cases:  
  - Balanced parentheses problems
  - Representing an arbitrary 2 dimensional walk

## GettingStarted
In `build.sbt` add the jitpack resolver:
```
resolvers += "jitpack" at "https://jitpack.io"
```

In `build.sbt` add the library dependency:
```
libraryDependencies += "com.github.nathaniel-may" % "nest" % "v0.1.0"
```

### Code Examples:
#### Construction:
```scala
import nest._

// all of type Nest[String, Boolean]
val n0 = Nest("hello", true)
val n1 = </>("hello", true)
val n2 = <\>(true, "hello")
val n3 = "hello" </: Nest.empty :/> true
val n4 = true <\: Nest.empty :\> "hello"
```

#### Pattern Matching:
```scala
import nest._

Nest("hello", true) match {
  case Nest.empty             => false
  case str  </: nest :/> bool => bool
  case bool <\: nest :\> str  => bool
}
```

#### Lists:
```scala
import nest._

val nest = ("a" </: ("b" </: Nest.empty :/> "c") :/> "d")
nest match {
  case Nest.empty => ""
  case s1 </: _ :/> s2 => s"$s1 $s2" // <--- matches here as "a d"
  case s1 <\: _ :\> s2 => s"$s1 $s2"
}
nest.toList // ---> List("a", "b", "c", "d")
```

## Future Improvements:
  - provide better map interface
  - improve performance of size and depth
  - implement scalacheck shrinker for nest
  - consider defaulting to lazy behavior