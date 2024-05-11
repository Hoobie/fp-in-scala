package chapter4

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)
 
  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e) => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_) => b
    case r => r

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- that
    } yield f(a, b)

object Either:
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    as.foldLeft(Right[E, List[A]](List[A]()))((acc, either) => acc.map2(either)(_ :+ _))
 
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(Right[E, List[B]](List[B]()))((acc, a) => acc.map2(f(a))(_ :+ _))
