package chapter3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

object Tree:
  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  // Exercise 3.25
  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lMax = l.maximum
      val rMax = r.maximum
      lMax.max(rMax)

  // Exercise 3.26
  extension (t: Tree[Int]) def depth(accCurr: Int = 0, accRes: Int = 0): Int = t match
    case Leaf(_) => accCurr.max(accRes)
    case Branch(l, r) => l.depth(accCurr + 1, accRes).max(r.depth(accCurr + 1, accRes))

  // Exercise 3.27
  extension [A](t: Tree[A]) def map[B](f: A => B): Tree[B] = t match
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  // Exercise 3.28
  extension [A](t: Tree[A]) def fold[B](f: (A, B) => B, acc: B): B = t match
    case Leaf(i) => f(i, acc)
    case Branch(l, r) => l.fold(f, r.fold(f,  acc))

  extension (t: Tree[Int]) def foldMaximum: Int = t.fold(_ max _, Int.MinValue)
