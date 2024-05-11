package chapter5

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if (n > 0) => Cons(h, () => t().take(n - 1))
    case _                     => Empty

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Empty          => Empty
    case c @ Cons(h, t) => if (n > 0) t().drop(n - 1) else c

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _                      => Empty

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def takeWhileViaFold(p: A => Boolean): LazyList[A] =
    foldRight(LazyList[A]())((a, acc) =>
      if (p(a)) LazyList.cons(a, acc) else acc
    )

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList[B]())((a, acc) => LazyList.cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList[A]())((a, acc) =>
      if (p(a)) LazyList.cons(a, acc) else acc
    )

  def append[B >: A](b: => LazyList[B]): LazyList[B] =
    foldRight(b)((a, acc) => LazyList.cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList[B]())((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this) { oldList =>
      oldList match
        case Empty      => None
        case Cons(h, t) => Some(f(h()) -> t())
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold(this -> n) { (oldList, i) =>
      oldList match
        case Empty       => None
        case _ if i == 0 => None
        case Cons(h, t)  => Some(h(), t() -> (i - 1))
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this) {
      case Empty                 => None
      case Cons(h, _) if !p(h()) => None
      case Cons(h, t)            => Some(h(), t())
    }

  def zipWith[B, C](b: LazyList[B], f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, b)) { state =>
      state match
        case (Empty, _)                   => None
        case (_, Empty)                   => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)) { state =>
      state match
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) =>
          Some((Some(h()), None) -> (t(), LazyList.empty[B]))
        case (Empty, Cons(h, t)) =>
          Some((None, Some(h())) -> (LazyList.empty, t()))
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipWith(prefix, _ == _).forAll(_ == true)

  def tails: LazyList[LazyList[A]] =
    LazyList
      .unfold(this) { state =>
        state match
          case Empty          => None
          case c @ Cons(_, t) => Some(c -> t())
      }
      .append(LazyList(LazyList.empty))

  // extra
  def scanLeft[B](acc: => B)(f: (A, => B) => B): LazyList[B] =
    LazyList
      .cons(
        acc,
        LazyList.unfold(this, acc) { state =>
          state match
            case (Empty, _) => None
            case (Cons(h, t), newAcc) =>
              val b = f(h(), newAcc)
              Some(b -> (t(), b))
        }
      )

  def scanRight[B](acc: => B)(f: (A, => B) => B): LazyList[B] =
    foldRight(LazyList[B](acc)) { (a, b) =>
      // can use .get as the accumulator isn't ever empty
      Cons(() => f(a, b.headOption.get), () => b)
    }

object LazyList:
  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def cons[A](
      hd: => A,
      tl: => LazyList[A]
  ): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    LazyList.cons(n, from(n + 1))

  def fibs(a: Int = 0, b: Int = 1): LazyList[Int] =
    LazyList.cons(a, fibs(b, a + b))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state)
      .map((a, newState) => LazyList.cons(a, unfold(newState)(f)))
      .getOrElse(LazyList.Empty)

  val onesViaUnfold: LazyList[Int] = unfold(1)((s: Int) => Some(s -> s))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)((state: A) => Some(state -> state))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)((state: Int) => Some((state, state + 1)))

  def fibsViaUnfold(a: Int = 0, b: Int = 1): LazyList[Int] =
    unfold(a -> b)((state: (Int, Int)) =>
      Some((state._1, (state._2, state._1 + state._2)))
    )
