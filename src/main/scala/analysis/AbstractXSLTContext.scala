package analysis

case class AbstractXSLTContext[N, L, T](
  node: N,
  nodeList: L,
  position: T,
  variables: Map[String, T]
) {
  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: T): AbstractXSLTContext[N, L, T] = {
    AbstractXSLTContext(node, nodeList, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, L, T] = {
    AbstractXSLTContext(node, nodeList, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, L, T] = {
    AbstractXSLTContext(node, nodeList, position, newVariables)
  }
}
