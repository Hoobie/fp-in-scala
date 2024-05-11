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

    def map2Timingout[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
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
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })
