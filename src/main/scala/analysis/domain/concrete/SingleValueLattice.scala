package analysis.domain.concrete

import analysis.{Incomparable, Less, Greater, Equal}

trait SingleValueLattice[+T] {
  def map[R](f: T => R): SingleValueLattice[R]

  def join[T1 >: T](other: SingleValueLattice[T1]): SingleValueLattice[T1] = (this, other) match {
    case (Top, _) => Top
    case (_, Top) => Top
    case (Value(v), Bottom) => Value(v)
    case (Bottom, Value(v)) => Value(v)
    case (Value(v1), Value(v2)) if v1 == v2 => this
    case _ => Bottom
  }

  def meet[T1 >: T](other: SingleValueLattice[T1]): SingleValueLattice[T1] = (this, other) match {
    case (Bottom, _) => Bottom
    case (_, Bottom) => Bottom
    case (Value(v), Top) => Value(v)
    case (Top, Value(v)) => Value(v)
    case (Value(v1), Value(v2)) if v1 == v2 => this
    case _ => Top
  }

  def liftBinaryOp[R, T1 >: T](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R] = (this, other) match {
    case (Value(v1), Value(v2)) => Value(f(v1, v2))
    case (Bottom, _) => Bottom
    case (_, Bottom) => Bottom
    case _ => Top
  }

  def lessThanOrEqual[T1 >: T](other: SingleValueLattice[T1]) = (this, other) match {
    case (Value(v1), Value(v2)) if v1 == v2 => true
    case (_, Top) => true
    case (Bottom, _) => true
    case _ => false
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