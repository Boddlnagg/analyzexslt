package analysis

case class AbstractXSLTContext[N, L, V](
  node: N,
  nodeList: L,
  position: V,
  variables: Map[String, V]
) {
  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: V): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, V]): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, V]): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, newVariables)
  }
}
