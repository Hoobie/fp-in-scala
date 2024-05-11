package chapter6

@main def main: Any =
  val rng = new SimpleRNG(1)
  println(RNG.nonNegativeInt(rng))
  println(RNG.nonNegativeInt(rng))
  println(RNG.intDouble(rng))
  println(RNG.doubleInt(rng))
  println(RNG.double3(rng))
  println(RNG.ints(5)(rng))
  println(RNG.doubleViaMap(rng))
  println(RNG.double2ViaBoth(rng))

  val state = State((s: Int) => (s + 1, s + 1))
  println(state.map(_ * 3).run(2))
  println(state.flatMap(s1 => State((s2 :Int) => (s1 + s2, s2))).run(2))

  val machine = Machine(locked = true, candies = 1, coins = 0)
  assert(Machine.process(machine, Input.Turn) == machine)
  val machine2 = Machine.process(machine, Input.Coin)
  assert(machine2 == machine.copy(locked = false, coins = 1))
  assert(Machine.process(machine2, Input.Turn) == Machine(locked = true, candies = 0, coins = 1))
  println(Machine.simulateMachine(List(Input.Coin, Input.Turn)).run(machine))
