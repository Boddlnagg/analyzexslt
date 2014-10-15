package analysis.domain.concrete

abstract class SingleValueLattice[T] {
  def map[R](f: T => R): SingleValueLattice[R] = this match {
    case Top() => Top()
    case Bottom() => Bottom()
    case Value(v) => Value(f(v))
  }

  def join(other: SingleValueLattice[T]): SingleValueLattice[T] = (this, other) match {
    case (Top(), _) => Top()
    case (_, Top()) => Top()
    case (Value(v), Bottom()) => Value(v)
    case (Bottom(), Value(v)) => Value(v)
    case _ => Bottom()
  }

  def liftBinaryOp[R](other: SingleValueLattice[T])(f: (T, T) => R): SingleValueLattice[R] = (this, other) match {
    case (Value(v1), Value(v2)) => Value(f(v1, v2))
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case _ => Top()
  }
}

// NOTE: the type systems requires us to provide a generic version of Top and Bottom here instead of `case object Top`
case class Top[T]() extends SingleValueLattice[T]
case class Bottom[T]() extends SingleValueLattice[T]
case class Value[T](obj: T) extends SingleValueLattice[T]
