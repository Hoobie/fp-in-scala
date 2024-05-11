package chapter4

import chapter4.Option.*
import chapter4.Either.*

@main def main: Any =
  val opt = Some(1)
  println(opt.map(_ + 1))
  println(opt.flatMap(Some(_)))
  println(opt.getOrElse(3))
  println(opt.orElse(Some(3)))
  println(opt.filter(_ > 2))
  println(Option.map2(Some(1), Some(2))(_ + _))
  println(Option.sequence(List(Some(1), Some(2), Some(3))))
  println(Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3))))

  val left = Left("error")
  val right = Right(1)
  println(right.map(_ + 1))
  println(right.flatMap(Right(_)))
  println(left.orElse(Right(2)))
  println(right.map2(Right(2))(_ + _))
  println(Either.sequence(List(Right(1), Right(2), Right(3))))
