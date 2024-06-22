package chapter8

import chapter6.State
import chapter6.RNG

// provided
opaque type Gen[+A] = State[RNG, A]

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State((r: RNG) => {
      val (a, s) = RNG.nonNegativeInt(r)
      start + (a % (stopExclusive - start)) -> s
    })

  def get[A](gen: Gen[A], rng: RNG): A = gen.run(rng)._1
}
