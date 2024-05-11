package chapter3

import Tree.*

@main def main: Unit =
  val list = List(1, 2, 3)
  println(List.tail(list))
  println(List.setHead(list, 4))
  println(List.drop(list, 2))
  println(List.dropWhile(list, _ <= 2))
  println(List.init(list))
  println(List.length(list))
  println(List.foldLeft(list, 0, _ + _))
  println(List.reverse(list))
  println(List.appendViaFoldLeft(list, list))
  println(List.foldRightViaFoldLeft(list, 0, _ + _))
  println(List.concat(List(list, list, list)))
  println(List.map(list, _ + 1))
  println(List.filter(list, _ % 2 == 0))
  println(List.flatMap(list, x => List(x, x)))
  println(List.filterViaFlatMap(list, _ % 2 == 0))
  println(List.addCorresponding(list, list))
  println(List.hasSubsequence(list, List(2, 3)))
  println(List.hasSubsequence(list, List(2, 2)))
  println(List.hasSubsequence(List(1, 1, 1, 2, 3), List(1, 1, 2)))

  val tree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(3), Branch(Leaf(5), Leaf(2))))
  println(tree)
  println(s"Max: ${tree.maximum}")
  println(s"Depth: ${tree.depth()}")
  println(tree.map(_ + 1))
  println(tree.fold[Int](_ + _, 0))
