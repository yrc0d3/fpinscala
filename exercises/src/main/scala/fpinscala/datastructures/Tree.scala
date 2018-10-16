package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // 3.25
  def size(t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth(t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  def map(t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold(t: Tree[A]): Int = fold(t)(_ = >1)(1 + _ + _)

  def maximumViaFold(t: Tree[A]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold(t: Tree[A]): Int = fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def map(t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Left(f(a)): Leaf[B])(Branch(_, _))
}