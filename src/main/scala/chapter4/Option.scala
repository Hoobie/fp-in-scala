package chapter4

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(a) => a
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

object Option:
  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)
 
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldLeft(Some(List[A]()))((acc, opt) => opt.flatMap(a => acc.map(l => l :+ a)))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(List[B]()))((acc, a) => f(a).flatMap(b => acc.map(l => l :+ b))) 

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)