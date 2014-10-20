package analysis.domain.powerset

import analysis._
import analysis.domain.{XPathDomain, XMLDomain}
import xml._

/** Just a wrapper for the type aliases */
object PowersetXMLDomain {
  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets
  type L = Option[Set[List[XMLNode]]]

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    override def top: N = None
    override def bottom: N = BOT

    protected val BOT: N = Some(Set())

    override def topList: L = None
    override def bottomList: L = BOT_LIST

    protected val BOT_LIST: L = Some(Set())

    override def join(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    /*override def meet(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }

    override def compare(morePrecise: N, lessPrecise: N): Boolean = (morePrecise, lessPrecise) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }*/

    override def compare(n1: N, n2: N): LatticeOrdering = (n1, n2) match {
      case (None, None) => Equal
      case (None, _) => Greater
      case (_, None) => Less
      case (Some(s1), Some(s2)) => (s1.subsetOf(s2), s2.subsetOf(s1)) match {
        case (true, true) => Equal
        case (true, false) => Less
        case (false, true) => Greater
        case (false, false) => Incomparable
      }
    }

    override def compareList(l1: L, l2: L): LatticeOrdering = (l1, l2) match {
      case (None, None) => Equal
      case (None, _) => Greater
      case (_, None) => Less
      case (Some(s1), Some(s2)) => (s1.subsetOf(s2), s2.subsetOf(s1)) match {
        case (true, true) => Equal
        case (true, false) => Less
        case (false, true) => Greater
        case (false, false) => Incomparable
      }
    }

    override def joinList(l1: L, l2: L): L = (l1, l2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    def createElement(name: String, attributes: L, children: L): N = {
      // TODO: simplify (remove helper functions that are able to do more than necessary)
      // appendChildren (taking a list of nodes) probably is better than appendChild applied multiple times (because of
      // combinatorial possibilities). We can still lift a single node to a list to append a single child
      def appendChildren(node: N, list: L): N = (node, list) match {
        case (Some(n), Some(l)) => Some(n.cross(l).map {
          // it can only be an XMLElement because we start with a single XMLElement and the type can't change
          case (e: XMLElement, ll) => {
            // copy the node, because each element of the set must refer to its own copy (because of the parent-child-references)
            assert(e.parent == null)
            val copy = e.copy.asInstanceOf[XMLElement]
            ll.foreach(newChild => {
              assert(newChild.parent == null)
              copy.appendChild(newChild.copy)
            })
            copy
          }
        }.toSet)
        case _ => None
      }

      def addAttributes(node: N, list: L): N = (node, list) match {
        case (Some(n), Some(l)) => Some(n.cross(l).map {
          // it can only be an XMLElement because we start with a single XMLElement and the type can't change
          case (e: XMLElement, ll) => {
            // copy the node, because each element of the set must refer to its own copy (because of the parent-child-references)
            assert(e.parent == null)
            val copy = e.copy.asInstanceOf[XMLElement]
            ll.foreach(newChild => {
              assert(newChild.parent == null)
              copy.addAttribute(newChild.copy.asInstanceOf[XMLAttribute])
            })
            copy
          }
        }.toSet)
        case _ => None
      }

      var result: N = Some(Set(XMLElement(name)))
      result = addAttributes(result, attributes)
      result = appendChildren(result, children)
      result
    }

    override def createEmptyList(): L = Some(Set(Nil))

    override def createSingletonList(node: N): L = node.map(_.map(n => List(n)))

    override def getRoot(node: N): N = node match {
      case None => None // infinite set of all possible roots (in this domain we can't express that it must be a root node)
      case Some(s) => Some(s.map(n => n.root))
    }

    override def getAttributes(node: N): L = node.map(_.map {
      case XMLElement(_, attr, _, _) => attr.toList
      case _ => Nil // NOTE: other node types have no attributes, but this must NOT evaluate to BOTTOM
    })

    override def getChildren(node: PowersetXMLDomain.N): L = node.map(_.map {
      case XMLRoot(elem) => List(elem)
      case XMLElement(_, _, children, _) => children.toList
      case _ => Nil // NOTE: other node types have no children, but this must NOT evaluate to BOTTOM
    })

    def getParent(node: N): N = node match {
      case None => None
      case Some(s) => Some(s.map(n => n.parent))
    }

    override def concatLists(list1: L, list2: L): L = (list1, list2) match {
      case (BOT_LIST, _) => BOT_LIST
      case (_, BOT_LIST) => BOT_LIST
      case (Some(l1), Some(l2)) => Some(l1.cross(l2).map {
        case (ll1, ll2) => ll1 ++ ll2
      }.toSet)
      case _ => None // at least one operand is TOP and the other is not BOTTOM
    }

    override def partitionAttributes(list: L): (L, L) = list match {
      case None => (None, None) // don't know anything about attributes or other nodes
      case Some(s) => val (attr, children) = s.map { l =>
        val resultAttributes = l.takeWhile(n => n.isInstanceOf[XMLAttribute])
        val resultChildren = l.filter(n => !n.isInstanceOf[XMLAttribute])
        (resultAttributes, resultChildren)
      }.unzip
      (Some(attr), Some(children))
    }

    override def wrapInRoot(list: L): N = list match {
      case None => None
      case Some(s) => Some(
        s.filter {
          case List(e: XMLElement) => true
          case l => println(f"[WARNING] Failed to wrap nodes in root: $l"); false
          // NOTE: Lists with more than one node or a non-element node are evaluated to bottom implicitly
        }.map {
          case List(e: XMLElement) => XMLRoot(e)
        }
      )
    }

    override def copyToOutput(list: L): L = list.map(_.map(_.map {
      case XMLRoot(elem) => elem.copy // "a root node is copied by copying its children" according to spec
      case node => node.copy
    }))

    override def getNodeListSize(list: L): V = list match {
      case None => xpathDom.topNumber
      case Some(s) => xpathDom.join(s.map(l => xpathDom.liftNumber(l.size)).toList)
    }

    override def getStringValue(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.join(s.map(n => xpathDom.liftLiteral(n.stringValue)).toList)
    }

    override def isRoot(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLRoot])
        (Some(yes), Some(no))
    }

    override def isElement(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLElement])
        (Some(yes), Some(no))
    }

    override def isTextNode(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLTextNode])
        (Some(yes), Some(no))
    }

    override def isComment(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLComment])
        (Some(yes), Some(no))
    }

    override def isAttribute(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLAttribute])
        (Some(yes), Some(no))
    }

    // first result is a node of which we KNOW that it matches
    // second result is a node of which we KNOW that it won't match
    override def hasName(node: N, name: String): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        // nodes that can't have a name are evaluated to bottom implicitly, i.e. they won't appear in the output at all
        val yes = s.filter {
          case XMLElement(elementName, _, _, _) => elementName == name
          case XMLAttribute(attributeName, _, _) => attributeName == name
          case _ => false
        }
        val no = s.filter {
        case XMLElement(elementName, _, _, _) => elementName != name
        case XMLAttribute(attributeName, _, _) => attributeName != name
        case _ => false
        }
        (Some(yes), Some(no))
    }

    override def hasParent(node: N, parent: N): (N, N) = (node, parent) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT) => (BOT, node) // parent is BOTTOM -> can't match
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), None) => (node, node) // parent is TOP -> don't know anything
      case ((Some(nodes), Some(parents))) =>
        val yes = nodes.filter(n => parents.contains(n.parent))
        val no = nodes.filter(n => !parents.contains(n.parent))
        (Some(yes), Some(no))
    }

    override def hasAncestor(node: N, ancestor: N): (N, N) = (node, ancestor) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT) => (BOT, node) // ancestor is BOTTOM -> can't match
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), None) => (node, node) // ancestor is TOP -> don't know anything
      case ((Some(nodes), Some(ancestors))) =>
        val yes = nodes.filter(n => ancestors.exists(a => n.ancestors.contains(a)))
        val no = nodes.filter(n => !ancestors.exists(a => n.ancestors.contains(a)))
        (Some(yes), Some(no))
    }

    // return empty string if node has no name
    override def getNodeName(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.join(s.map {
        case XMLElement(nodeName, _, _, _) => xpathDom.liftLiteral(nodeName)
        case XMLAttribute(nodeName, _, _) => xpathDom.liftLiteral(nodeName)
        case _ => xpathDom.liftLiteral("")
      }.toList)
    }

    override def getConcatenatedTextNodeValues(list: L): V = list match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.join(s.map { l =>
        xpathDom.liftLiteral(l.collect { case n: XMLTextNode => n.value }.mkString(""))
      }.toList)
    }

    override def filter(list: L, predicate: N => (N, N)): L = list match {
      case None => None
      case Some(s) => Some(s.map(_.filter { n =>
        val node: N = Some(Set(n))
        val (resultTrue, _) = predicate(node)
        // TODO: this could be made more abstract using compare operations (e.g. assert that resultTrue <= node)
        assert(resultTrue.isDefined)
        resultTrue.get.toList match {
          case Nil => false // list without elements -> element was filtered out
          case first :: Nil => true // list with one element -> element was not filtered out
          case _ =>
            // list with more than one element -> this should not happen in this domain
            throw new AssertionError("Filter predicate returned node with more than one possibility.")
        }
      }))
    }

    override def flatMapWithIndex(list: L, f: (N, V) => L): L = list match {
      case None => None
      case Some(s) => joinList(s.map { l =>
        val mapped = l.zipWithIndex.map { case (n, i) => f(Some(Set(n)), xpathDom.liftNumber(i)) }
        val flattened = mapped.foldLeft(createEmptyList())((acc, next) => concatLists(acc, next))
        flattened
      }.toList)
    }
  }
}
