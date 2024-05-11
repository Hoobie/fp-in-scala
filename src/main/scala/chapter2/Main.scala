package chapter2

@main def main: Unit =
  println(Exercise1.fib2(4))
  println(Exercise2.isSorted(Array(1, 2, 3), _ > _))
  val f = Exercise3.curry[Int, Int, Int](_ * _)
  println(f(2)(3))
  println(Exercise4.uncurry[Int, Int, Int](f)(2, 3))

  val g = (x: Double) => math.Pi / 2 - x
  val cos = Exercise5.compose(math.sin, g)
  println(cos(math.Pi / 3))
