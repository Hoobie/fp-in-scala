package chapter8

import chapter6.SimpleRNG

@main def main: Any =
  val rng = new SimpleRNG(4)
  println(Gen.get(Gen.choose(5, 8), rng))
  println(Gen.get(Gen.boolean, rng))
  println(Gen.get(Gen.boolean.listOfN(5), rng))
  println(Gen.get(Gen.union(Gen.choose(1, 4), Gen.boolean), rng))
  println(
    Gen.get(Gen.weighted(Gen.choose(1, 4) -> 0.01, Gen.boolean -> 0.99), rng)
  )

  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.forall(_ <= max)

  maxProp.run()

  val sortProp = Prop.forAll(smallInt.list)(l => l.sorted == l.sorted.sorted) &&
    Prop.forAll(smallInt.nonEmptyList) { l =>
      if (l.isEmpty)
        l.sorted.isEmpty
      else if (l.sizeIs == 1)
        l.sorted == l
      else
        l.zip(Int.MaxValue :: l.tail)
          .map { case (a, b) => a <= b }
          .reduce(_ && _)
    } && Prop.forAll(smallInt.list)(l => l.forall(l.sorted.contains))

  sortProp.run()
