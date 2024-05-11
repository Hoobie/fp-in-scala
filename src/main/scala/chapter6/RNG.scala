package chapter6

// provided
trait RNG:
  def nextInt: (Int, RNG)

// provided
case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object RNG:
  type Rand[+A] = RNG => (A, RNG)

  // provided
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // provided
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      f(a) -> rng2

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      f(a, b) -> rng3

  // provided
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    if (n == Int.MinValue) Int.MaxValue -> rng2
    else Math.abs(n) -> rng2

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    n / (Int.MaxValue.toDouble + 1) -> rng2

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    (i, d) -> rng3

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), rng2) = intDouble(rng)
    (d, i) -> rng2

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = RNG.double(rng)
    val (d2, rng3) = RNG.double(rng2)
    val (d3, rng4) = RNG.double(rng3)
    (d1, d2, d3) -> rng4

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    List.fill(count)(0).foldLeft(List[Int]() -> rng) { case ((acc, rng), _) =>
      val (n, rng2) = rng.nextInt
      (n :: acc) -> rng2
    }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def double2ViaBoth: Rand[(Double, Double)] =
    both(double, double)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft(unit(List[A]()))((acc, rand) => map2(acc, rand)(_ :+ _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)((rng: RNG) => rng.nextInt))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
