package chapter8

import chapter6.SimpleRNG

@main def main: Any =
  val rng = new SimpleRNG(4)
  println(Gen.get(Gen.choose(5, 8), rng))
