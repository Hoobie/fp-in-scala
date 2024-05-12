package chapter7

import java.util.concurrent.Executors

@main def main: Any =
  val es = Executors.newWorkStealingPool()
  println(Par.max(IndexedSeq(1, 5, 2)).run(es).get)
  println(Par.words(List("alice bob carl", "dog cat", "home", "tree")).run(es).get)
  println(Par.unit(1).map3(Par.unit(2), Par.unit(3))(_ + _ + _).run(es).get)
