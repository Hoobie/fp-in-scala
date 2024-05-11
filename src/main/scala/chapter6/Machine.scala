package chapter6

enum Input:
  case Coin, Turn

type Output = (Int, Int)

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine:
  def process(machine: Machine, input: Input): Machine = {
    (input, machine) match {
      case (Input.Coin, Machine(true, candies, coins)) if candies > 0 =>
        Machine(false, candies, coins + 1)
      case (Input.Turn, Machine(false, candies, coins)) if candies > 0 =>
        Machine(true, candies - 1, coins)
      case (_, machine) =>
        machine
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(
        inputs.map(input => State.modify(s => process(s, input)))
      )
      s <- State.get
    } yield (s.coins, s.candies)
