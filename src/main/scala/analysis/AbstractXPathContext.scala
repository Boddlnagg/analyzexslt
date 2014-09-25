package analysis

case class AbstractXPathContext[N, L, T](
  node: N,
  position: T,
  size: T,
  variables: Map[String, T]
)