package chapter2

import scala.annotation.tailrec

object Exercise1:
  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case n => fib(n - 2) + fib(n - 1)
    }

  def fib2(n: Int): Int =
    @tailrec def go(n: Int, acc1: Int = 0, acc2: Int = 1): Int =
      n match {
        case 0 => acc1
        case 1 => acc2
        case n => go(n - 1, acc2, acc1 + acc2)
      }
    go(n)
