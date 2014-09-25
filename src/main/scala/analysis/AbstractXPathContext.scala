package analysis

case class AbstractXPathContext[N, L, V](
  node: N,
  position: V,
  size: V,
  variables: Map[String, V]
)