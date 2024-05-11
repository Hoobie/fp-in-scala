package chapter3

import math.Numeric.Implicits.infixNumericOps
import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def tail[A](list: List[A]): Either[String, List[A]] =
    list match
      case Nil           => Left("List is empty")
      case Cons(_, rest) => Right(rest)

  def setHead[A](list: List[A], newHead: A): List[A] =
    list match
      case Nil           => List(newHead)
      case Cons(_, tail) => Cons(newHead, tail)

  def drop[A](as: List[A], n: Int): List[A] =
    if (n > 0)
      as match
        case Nil           => Nil
        case Cons(_, tail) => drop(tail, n - 1)
    else as

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case res @ Cons(head, tail) =>
        if (f(head))
          dropWhile(tail, f)
        else res

  def init[A](as: List[A]): List[A] =
    as match
      case Nil              => Nil
      case Cons(head, Nil)  => Nil
      case Cons(head, tail) => Cons(head, init(tail))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A](), (acc, a) => Cons(a, acc))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, (a, acc) => Cons(a, acc))
  
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (acc, a) => f(a, acc))

  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, List[A](), (a, acc) => a match {
      case Nil => acc
      case c: Cons[A] => List.append(c, acc)
    })

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, List[B](), (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List[B](), (a, acc) => List.append(f(a), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if(f(a)) List(a) else Nil)

  def addCorresponding[A: Numeric](xs: List[A], ys: List[A], acc: List[A] = Nil): Either[String, List[A]] =
    (xs, ys) match
      case (Nil, Nil) => Right(acc)
      case (Nil, _: Cons[A]) | (_: Cons[A], Nil) => Left("Incompatible lists")
      case (Cons(h1, t1), Cons(h2, t2)) => addCorresponding(t1, t2, List.append(acc, List(h1 + h2)))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    // 1 | (1, 1!, 2), 3
    // accSupGlobal: 1,1,1,2,3 accSupLocal: empty accSub: 1,1,2
    // accSupGlobal: 1,1,2,3   accSupLocal: 1     accSub: 1,2
    // accSupGlobal: 1,1,2,3   accSupLocal: 1,1   accSub: 2
    // accSupGlobal: 1,2,3     accSupLocal: 1     accSub: 1,1,2
    // ...

    def resetArgs(accSupGlobal: List[A]): (List[A], List[A], List[A]) =
      accSupGlobal match
        case Nil => (Nil, Nil, Nil)
        case Cons(head, tail) => (tail, accSupGlobal, sub)

    @tailrec 
    def go(accSupGlobal: List[A], accSupLocal: List[A], accSub: List[A]): Boolean =
      (accSupLocal, accSub) match
        case (Nil, Nil) => false // special case from reset
        case (Nil, _) =>
          // we ran out of the first list
          val args = resetArgs(accSupGlobal)
          go(args._1, args._2, args._3)
        case (_, Nil) =>
          // we matched all elements from the second list
          true
        case (Cons(last1, Nil), Cons(last2, Nil)) =>
          // prevent from running into the special case
          last1 == last2
        case (Cons(head1, tail1), Cons(head2, tail2)) =>
          if (head1 == head2)
            go(accSupGlobal, tail1, tail2)
          else 
            val args = resetArgs(accSupGlobal)
            go(args._1, args._2, args._3)

    val args = resetArgs(sup)
    go(args._1, args._2, args._3)
          