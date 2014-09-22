package analysis.domain

import xml._
import xslt.{XSLTTemplate, XSLTStylesheet}
import xpath._

import scala.collection.immutable.TreeSet

object PowersetXMLDomain {
  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets

  object D extends XMLDomain[N] {
    override def top: N = None
    override def bottom: N = Some(Set())

    override def lift(n: XMLNode): N = Some(Set(n))

    override def compare(morePrecise: N, lessPrecise: N): Boolean = (morePrecise, lessPrecise) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }

    override def chooseTemplates(sheet: XSLTStylesheet, n: N): Set[XSLTTemplate] = n match {
      // don't know anything -> return set of all matchable templates
      case None => sheet.matchableTemplates.map { case (_, tmpl, _, _) => tmpl }.toSet
      case Some(s) => s.map { node =>
        def allMatching = sheet.matchableTemplates.filter { case (tmpl, _, _, _) => XPathMatcher.matches(node, tmpl)}
        val (_, template, _, _) = allMatching.last // this one will have highest precedence and priority, because the templates are sorted
        template
      }
    }

    override def getRoot(node: N): N = node match {
      case None => None // infinite set of all possible roots (in this domain we can't express that it must be a root node)
      case Some(s) => Some(s.map(n => n.root))
    }

    def evaluateLocationPath(ctxNode: N, steps: List[XPathStep], isAbsolute: Boolean) = ???

    /** Evaluates the steps of a location path for a single (concrete) node. This is copied from XPathEvaluator.
      *
      * @param ctxNode the context node
      * @param steps the list of remaining steps to evaluate
      * @param isAbsolute a value indicating whether the location path is absolute (or relative)
      * @return an ordered set of nodes resulting from the location path, ordered in document order
      */
    def evaluateLocationPathSingle(ctxNode: XMLNode, steps: List[XPathStep], isAbsolute: Boolean): TreeSet[XMLNode] = {
      // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
      (steps, isAbsolute) match {
        case (Nil, true) => TreeSet(ctxNode.root)
        case (steps, true) => evaluateLocationPathSingle(ctxNode.root, steps, false)
        case (first :: rest, false) =>
          val nodes: TreeSet[XMLNode] = first.axis match {
            // the child axis contains the children of the context node
            case ChildAxis => ctxNode match {
              case XMLRoot(elem) => TreeSet(elem)
              case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children
              case _ => TreeSet()
            }
            // the descendant axis contains the descendants of the context node
            // a descendant is a child or a child of a child and so on
            case DescendantAxis => TreeSet[XMLNode]() ++ ctxNode.descendants
            // the parent axis contains the parent of the context node, if there is one
            case ParentAxis => ctxNode match {
              case XMLRoot(_) => TreeSet() // root does not have a parent
              case node => TreeSet(node.parent)
            }
            // the ancestor axis contains the ancestors of the context node
            // the ancestors of the context node consist of the parent of context node and the parent's parent and so on
            case AncestorAxis => TreeSet[XMLNode]() ++ ctxNode.ancestors
            // the following-sibling axis contains all the following siblings of the context node
            // if the context node is an attribute node or namespace node, the following-sibling axis is empty
            case FollowingSiblingAxis => ctxNode match {
              case XMLAttribute(_, _, _) => TreeSet()
              case _ => ctxNode.parent match {
                case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
                case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ > ctxNode)
              }
            }
            // the preceding-sibling axis contains all the preceding siblings of the context node
            // if the context node is an attribute node or namespace node, the preceding-sibling axis is empty
            case PrecedingSiblingAxis => ctxNode match {
              case XMLAttribute(_, _, _) => TreeSet()
              case _ => ctxNode.parent match {
                case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
                case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ < ctxNode)
              }
            }
            // the following axis contains all nodes in the same document as the context node that are after the context
            // node in document order, excluding any descendants and excluding attribute nodes and namespace nodes
            case FollowingAxis =>
              val descendants = ctxNode.descendants
              TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n > ctxNode && !descendants.contains(n))
            // the preceding axis contains all nodes in the same document as the context node that are before the context
            // node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes
            case PrecedingAxis =>
              val ancestors = ctxNode.ancestors
              TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n < ctxNode && !ancestors.contains(n))
            // the attribute axis contains the attributes of the context node; the axis will be empty
            // unless the context node is an element
            case AttributeAxis => ctxNode match {
              case XMLElement(_, attr, _, _) => TreeSet[XMLNode]() ++ attr
            }
            // the namespace axis contains the namespace nodes of the context node
            // the axis will be empty unless the context node is an element
            case NamespaceAxis => throw new NotImplementedError("Namespace nodes are not implemented, therefore the namespace axis is not supported")
            // the self axis contains just the context node itself
            case SelfAxis => TreeSet(ctxNode)
            // the descendant-or-self axis contains the context node and the descendants of the context node
            case DescendantOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.descendants
            // the ancestor-or-self axis contains the context node and the ancestors of the context node
            // thus, the ancestor axis will always include the root node
            case AncestorOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.ancestors
          }
          val testedNodes = nodes.filter {node => first.test match {
            case NameTest("*") => XPathAxis.isPrincipalNodeType(first.axis, node)
            case NameTest(testName) => XPathAxis.isPrincipalNodeType(first.axis, node) && (node match {
              case XMLElement(name, _, _, _) => name == testName
              case XMLAttribute(name, _, _) => name == testName
              case _ => false
            })
            case TextNodeTest => node.isInstanceOf[XMLTextNode]
            case CommentNodeTest => node.isInstanceOf[XMLComment]
            case AllNodeTest => true
          }}
          if (!first.predicates.isEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
          testedNodes.flatMap { n => evaluateLocationPathSingle(n, rest, false)}
        case (Nil, false) => TreeSet(ctxNode)
      }
    }

    // TODO: make this more abstract and move it back to XPathAnalyzer (use T with NodeSetValues instead of TreeSet[XMLNode])?
    // currently this does not type check
    /*def evaluateLocationPathAbstract(ctxNode: N, steps: List[XPathStep], isAbsolute: Boolean): Option[Set[TreeSet[XMLNode]]] = {
      // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
      (steps, isAbsolute) match {
        case (Nil, true) => ctxNode match {
          case None => None
          case Some(s) => Some(s.map(n => TreeSet[XMLNode](n.root)))
        }
        case (steps, true) => evaluateLocationPathInternalAbstract(getRoot(ctxNode), steps, false)
        case (first :: rest, false) =>
          val nodes: Option[Set[TreeSet[XMLNode]]] = first.axis match {
            // the child axis contains the children of the context node
            case ChildAxis => ctxNode match {
              case None => None // know nothing about the node -> know nothing about it's children
              case Some(s) => Some(s.map(n => n match {
                case XMLRoot(elem) => TreeSet[XMLNode](elem)
                case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children
                case _ => TreeSet[XMLNode]()
              }))
            }
            // the descendant axis contains the descendants of the context node
            // a descendant is a child or a child of a child and so on
            /*case DescendantAxis => TreeSet[XMLNode]() ++ ctxNode.descendants
            // the parent axis contains the parent of the context node, if there is one
            case ParentAxis => ctxNode match {
              case XMLRoot(_) => TreeSet() // root does not have a parent
              case node => TreeSet(node.parent)
            }
            // the ancestor axis contains the ancestors of the context node
            // the ancestors of the context node consist of the parent of context node and the parent's parent and so on
            case AncestorAxis => TreeSet[XMLNode]() ++ ctxNode.ancestors
            // the following-sibling axis contains all the following siblings of the context node
            // if the context node is an attribute node or namespace node, the following-sibling axis is empty
            case FollowingSiblingAxis => ctxNode match {
              case XMLAttribute(_, _, _) => TreeSet()
              case _ => ctxNode.parent match {
                case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
                case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ > ctxNode)
              }
            }
            // the preceding-sibling axis contains all the preceding siblings of the context node
            // if the context node is an attribute node or namespace node, the preceding-sibling axis is empty
            case PrecedingSiblingAxis => ctxNode match {
              case XMLAttribute(_, _, _) => TreeSet()
              case _ => ctxNode.parent match {
                case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
                case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ < ctxNode)
              }
            }
            // the following axis contains all nodes in the same document as the context node that are after the context
            // node in document order, excluding any descendants and excluding attribute nodes and namespace nodes
            case FollowingAxis =>
              val descendants = ctxNode.descendants
              TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n > ctxNode && !descendants.contains(n))
            // the preceding axis contains all nodes in the same document as the context node that are before the context
            // node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes
            case PrecedingAxis =>
              val ancestors = ctxNode.ancestors
              TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n < ctxNode && !ancestors.contains(n))
            // the attribute axis contains the attributes of the context node; the axis will be empty
            // unless the context node is an element
            case AttributeAxis => ctxNode match {
              case XMLElement(_, attr, _, _) => TreeSet[XMLNode]() ++ attr
            }
            // the namespace axis contains the namespace nodes of the context node
            // the axis will be empty unless the context node is an element
            case NamespaceAxis => throw new NotImplementedError("Namespace nodes are not implemented, therefore the namespace axis is not supported")
            // the self axis contains just the context node itself
            case SelfAxis => TreeSet(ctxNode)
            // the descendant-or-self axis contains the context node and the descendants of the context node
            case DescendantOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.descendants
            // the ancestor-or-self axis contains the context node and the ancestors of the context node
            // thus, the ancestor axis will always include the root node
            case AncestorOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.ancestors*/
          }
          val testedNodes = nodes.map(_.map(s => s.filter {node => first.test match {
            case NameTest("*") => XPathAxis.isPrincipalNodeType(first.axis, node)
            case NameTest(testName) => XPathAxis.isPrincipalNodeType(first.axis, node) && (node match {
              case XMLElement(name, _, _, _) => name == testName
              case XMLAttribute(name, _, _) => name == testName
              case _ => false
            })
            case TextNodeTest => node.isInstanceOf[XMLTextNode]
            case CommentNodeTest => node.isInstanceOf[XMLComment]
            case AllNodeTest => true
          }}))
          if (!first.predicates.isEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
          testedNodes match {
            case None => None
            case Some(s) => Some(s.map(nodes => nodes.flatMap { n => evaluateLocationPathAbstract(lift(n), rest, false)}))
           //.flatMap { n => evaluateLocationPath(n, rest, false)}
          }
        case (Nil, false) => ctxNode match {
          case None => None
          case Some(s) => Some(s.map(n => TreeSet(n)))
        }
      }
    }*/

    override def join(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    override def meet(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }
  }
}
