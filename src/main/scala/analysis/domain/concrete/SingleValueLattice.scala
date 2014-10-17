package analysis.domain.concrete

import analysis.{Incomparable, Less, Greater, Equal}

trait SingleValueLattice[+T] {
  def map[R](f: T => R): SingleValueLattice[R]

  def join[T1 >: T](other: SingleValueLattice[T1]): SingleValueLattice[T1] = (this, other) match {
    case (Top, _) => Top
    case (_, Top) => Top
    case (Value(v), Bottom) => Value(v)
    case (Bottom, Value(v)) => Value(v)
    case (Value(v1), Value(v2)) if v1 == v2 => Value(v1)
    case _ => Bottom
  }

  def liftBinaryOp[R, T1 >: T](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R] = (this, other) match {
    case (Value(v1), Value(v2)) => Value(f(v1, v2))
    case (Bottom, _) => Bottom
    case (_, Bottom) => Bottom
    case _ => Top
  }

  def compare[T1 >: T](other: SingleValueLattice[T1]) = (this, other) match {
    case (_, _) if this == other => Equal
    case (Top, _) => Greater
    case (Bottom, _) => Less
    case (_, Top) => Less
    case (_, Bottom) => Greater
    case _ => Incomparable
  }
}

case object Top extends SingleValueLattice[Nothing] {
  def map[R](f: Nothing => R): SingleValueLattice[R] = Top
}

case object Bottom extends SingleValueLattice[Nothing] {
  def map[R](f: Nothing => R): SingleValueLattice[R] = Bottom
}

case class Value[T](obj: T) extends SingleValueLattice[T] {
  def map[R](f: T => R): SingleValueLattice[R] = Value(f(obj))
}