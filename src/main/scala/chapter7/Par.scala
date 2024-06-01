package chapter7

import java.util.concurrent.TimeUnit
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.Callable

// provided
opaque type Par[A] = ExecutorService => Future[A]

// provided
extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

object Par:
  // provided
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  // provided
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(Par.unit(List.empty[A])) { case (acc, pa) =>
      acc.map2(pa)(_ :+ _)
    }

  // provided
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork:
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      parMap(as){ a =>
        if (f(a)) List(a) else List.empty
      }.map(_.flatten)

  // provided
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)

  def parFold[A](seq: IndexedSeq[A], default: A)(combine: (A, A) => A): Par[A] =
    if seq.size <= 1 then Par.unit(seq.headOption.getOrElse(default))
    else
      val (l, r) = seq.splitAt(seq.size / 2)
      Par
        .fork(parFold(l, default)(combine))
        .map2(Par.fork(parFold(r, default)(combine)))(combine)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    parFold(ints, Int.MinValue)(Math.max)

  def words(paragraphs: List[String]): Par[Long] =
    fork:
      parMap(paragraphs)(_.split(" ").length).map(_.sum)

  def parCompute[A, B](items: List[A])(f: A => B)(combine: List[B] => B) =
    fork:
      parMap(items)(f).map(combine)

  // provided
  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A])
    // provided
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

    def map2TimingOut[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C] {
          val fa = pa(es)
          val fb = pb(es)

          override def cancel(mayInterruptIfRunning: Boolean): Boolean =
            fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)

          override def isCancelled(): Boolean =
            fa.isCancelled() && fb.isCancelled()

          override def isDone(): Boolean = fa.isDone() && fb.isDone()

          override def get(timeout: Long, unit: TimeUnit): C = {
            val start = System.nanoTime()
            val a = fa.get(timeout, unit)
            val elapsed = System.nanoTime() - start
            val remains = unit.toNanos(timeout) - elapsed
            val b = fb.get(remains, TimeUnit.NANOSECONDS)
            f(a, b)
          }

          override def get(): C = f(fa.get, fb.get)
        }

    // provided
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def map3[B, C, D](pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
      pa.map2(pb)(_ -> _).map2(pc) {
        case ((a, b), c) => f(a, b, c)
      }
