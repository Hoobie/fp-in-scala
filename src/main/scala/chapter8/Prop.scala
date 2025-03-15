package chapter8

import chapter6.RNG
import chapter6.SimpleRNG

// provided
opaque type Prop = (MaxSize, TestCases, RNG) => Result

// provided
opaque type MaxSize = Int
object MaxSize:
  extension (x: MaxSize) def toInt: Int = x
  def fromInt(x: Int): MaxSize = x

extension (self: Prop)
  def &&(that: Prop): Prop =
    (max, n, rng) =>
      self(max, n, rng) match {
        case Result.Passed       => that(max, n, rng)
        case f: Result.Falsified => f
      }

  def ||(that: Prop): Prop =
    (max, n, rng) =>
      self(max, n, rng) match {
        case p @ Result.Passed      => p
        case Result.Falsified(_, _) => that(max, n, rng)
      }

  // provided
  def run(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit =
    self(maxSize, testCases, rng) match
      case Result.Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Result.Passed =>
        println(s"+ OK, passed $testCases tests.")

object Prop:
  // provided
  def forAll[A](gen: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList
          .from(0)
          .take((n.toInt min max.toInt) + 1)
          .map(i => forAll(gen(i))(f))
      val prop: Prop =
        props
          .map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng))
          .toList
          .reduce(_ && _)
      prop(max, n, rng)

  // provided
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Result.Passed
              else Result.Falsified(a.toString, i)
            catch
              case e: Exception =>
                Result.Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Result.Passed)

  // provided
  def randomLazyList[A](gen: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(gen.run(rng)))

  // provided
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

// provided
opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

// provided
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed          => false
    case Falsified(_, _) => true

// provided
opaque type FailedCase = String
opaque type SuccessCount = Int

// trait Prop { self =>
//   def check: Boolean

//   def &&(that: Prop): Prop =
//     new Prop {
//       def check = this.check && that.check
//     }
// }
