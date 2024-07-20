package chapter8

import chapter6.State
import chapter6.RNG

// provided
opaque type Gen[+A] = State[RNG, A]

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State { rng =>
      val (a, s) = RNG.nonNegativeInt(rng)
      start + (a % (stopExclusive - start)) -> s
    }

  def get[A](gen: Gen[A], rng: RNG): A = gen.run(rng)._1

  def unit[A](a: => A): Gen[A] = State(r => a -> r)

  def boolean: Gen[Boolean] =
    State { rng =>
      val (a, s) = rng.nextInt
      (a >= 0) -> s
    }

  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      self.flatMap(f)

  extension [A](self: Gen[A])
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(bool => if (bool) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    require(g1._2 > 0 && g1._2 < 1)
    require(g2._2 > 0 && g2._2 < 1)
    require(g1._2 + g2._2 == 1.0)
    State { (rng: RNG) =>
      val ((d1, d2), s) = RNG.double2ViaBoth(rng)
      val doubles = Seq(d1, d2)
      val ratio = doubles.min / doubles.max
      ratio -> s
    }.flatMap { ratio =>
      val gens = Seq(g1, g2).sortBy(_._2)
      if (gens.head._2 < ratio) gens.head._1 else gens(1)._1
    }
  }
}
