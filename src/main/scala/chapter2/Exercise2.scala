package chapter2

object Exercise2:
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    as.grouped(2).forall {
      case Array(a, b) => !gt(a, b)
      case Array(a) => true
      case _ => throw IllegalStateException("impossible")
    }
