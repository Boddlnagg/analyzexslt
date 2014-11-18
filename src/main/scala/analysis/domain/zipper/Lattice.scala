package analysis.domain.zipper

trait Lattice[A] {
  def top: A
  def bottom: A
  def join(left: A, right: A): A
  def meet(left: A, right: A): A
}
