package main.scala.funsets

object Main extends App {
  import FunSets._
  printSet(union(singletonSet(3), singletonSet(8)))
  printSet(map(union(singletonSet(3), singletonSet(8)), x => x * 2))
}
