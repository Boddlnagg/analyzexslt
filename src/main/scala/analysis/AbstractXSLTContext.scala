package analysis

case class AbstractXSLTContext[N, L, V](
  node: N,
  nodeList: L,
  position: V,
  globalVariables: Map[String, V],
  localVariables: Map[String, V]
) {
  val variables = globalVariables ++ localVariables

  /** Returns a new context where a local variable binding was added to the set of variables */
  def addLocalVariable(name: String, value: V): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, globalVariables, localVariables + (name -> value))
  }

  /** Returns a new context where a number of local variable bindings were added to the set of variables */
  def addLocalVariables(newVariables: Map[String, V]): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, globalVariables, localVariables ++ newVariables)
  }

  /** Returns a new context where the local variable bindings were replaced by the given new set of variable */
  def replaceLocalVariables(newVariables: Map[String, V]): AbstractXSLTContext[N, L, V] = {
    AbstractXSLTContext(node, nodeList, position, globalVariables, newVariables)
  }
}
