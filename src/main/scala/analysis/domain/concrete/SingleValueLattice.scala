package analysis.domain.concrete

trait SingleValueLattice[+T] {
  def map[R](f: T => R): SingleValueLattice[R]
  def join[T1 >: T](other: SingleValueLattice[T1]): SingleValueLattice[T1]
  def liftBinaryOp[R, T1 >: T](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R]
}

case object Top extends SingleValueLattice[Nothing] {
  def map[R](f: Nothing => R): SingleValueLattice[R] = Top
  def join[T1 >: Nothing](other: SingleValueLattice[T1]): SingleValueLattice[T1] = Top
  def liftBinaryOp[R, T1 >: Nothing](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R] = other match {
    case Bottom => Bottom
    case _ => Top
  }
}
case object Bottom extends SingleValueLattice[Nothing] {
  def map[R](f: Nothing => R): SingleValueLattice[R] = Bottom
  def join[T1 >: Nothing](other: SingleValueLattice[T1]): SingleValueLattice[T1] = other match {
    case Top => Top
    case Bottom => Bottom
    case Value(v) => Value(v)
  }
  def liftBinaryOp[R, T1 >: Nothing](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R] = Bottom
}
case class Value[T](obj: T) extends SingleValueLattice[T] {
  def map[R](f: T => R): SingleValueLattice[R] = Value(f(obj))
  def join[T1 >: T](other: SingleValueLattice[T1]): SingleValueLattice[T1] = other match {
    case Top => Top
    case Bottom => this
    case Value(v) if v == obj => this // same value
    case _ => Top // another value
  }
  def liftBinaryOp[R, T1 >: T](other: SingleValueLattice[T1])(f: (T1, T1) => R): SingleValueLattice[R] = other match {
    case Bottom => Bottom
    case Top => Top
    case Value(v) => Value(f(obj, v))
  }
}