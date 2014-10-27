package analysis.domain.powerset

import analysis.domain.{XPathDomain, XMLDomain}
import xml._

/** Just a wrapper for the type aliases */
object PowersetXMLDomain {
  type N = Either[Boolean, Set[XMLNode]] // Left(true) represents the infinite set of root-nodes;
                                         // Left(false) represents any infinite set; Right represents finite sets
  type L = Either[Option[Int], Set[List[XMLNode]]] // Left(Some(n)) is a list with n unknown elements (n > 0)

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    override def top: N = Left(false)
    override def bottom: N = BOT

    protected val BOT: N = Right(Set())

    override def topList: L = Left(None)
    override def bottomList: L = BOT_LIST

    protected val BOT_LIST: L = Right(Set())

    override def join(n1: N, n2: N): N = (n1, n2) match {
      case (Left(r1), Left(r2)) => Left(r1 && r2)
      case (Left(_), _) => Left(false)
      case (_, Left(_)) => Left(false)
      case (Right(s1), Right(s2)) => Right(s1.union(s2))
    }

    override def meet(n1: N, n2: N): N = (n1, n2) match {
      case (Left(r1), Left(r2)) => Left(r1 || r2)
      case (Left(_), _) => n2
      case (_, Left(_)) => n1
      case (Right(s1), Right(s2)) => Right(s1.intersect(s2))
    }

    override def lessThanOrEqual(n1: N, n2: N): Boolean = (n1, n2) match {
      case (_, Left(false)) => true
      case (Left(false), Left(true)) => false
      case (Left(true), Left(true)) => true
      case (Right(_), Left(_)) => true
      case (Left(_), Right(_)) => false
      case (Right(s1), Right(s2)) => s1.subsetOf(s2)
    }

    override def lessThanOrEqualList(l1: L, l2: L): Boolean = (l1, l2) match {
      case (_, Left(None)) => true
      case (Left(Some(len1)), Left(Some(len2))) => len1 == len2
      case (Left(None), Left(Some(_))) => false
      case (Right(_), Left(_)) => true
      case (Left(_), Right(_)) => false
      case (Right(s1), Right(s2)) => s1.subsetOf(s2)
    }

    override def joinList(l1: L, l2: L): L = (l1, l2) match {
      case (Left(Some(len1)), Left(Some(len2))) if len1 == len2 => l1
      case (Left(_), Left(_)) => Left(None)
      case (Left(_), Right(_)) => l1
      case (Right(_), Left(_)) => l2
      case (Right(s1), Right(s2)) => Right(s1.union(s2))
    }

    def createElement(name: String, attributes: L, children: L): N = (attributes, children) match {
      case (Left(_), _) => Left(false)
      case (_, Left(_)) => Left(false)
      case (Right(s1), Right(s2)) => Right(s1.cross(s2).map {
        case (attr, chld) => XMLElement(name,
          attr.map(a => a.asInstanceOf[XMLAttribute].copy),
          chld.map(c => c.copy))
      }.toSet)
    }

    override def createEmptyList(): L = Right(Set(Nil))

    override def createSingletonList(node: N): L = node match {
      case Left(_) => Left(Some(1))
      case Right(s) => Right(s.map(n => List(n)))
    }

    override def getRoot(node: N): N = node match {
      case Left(_) => Left(false) // infinite set of all possible roots
      case Right(s) => Right(s.map(n => n.root))
    }

    override def getAttributes(node: N): L = node match {
      case Left(true) => Right(Set(Nil)) // root nodes don't have attributes
      case Left(false) => Left(None)
      case Right(s) => Right(s.map {
        case XMLElement(_, attr, _, _) => attr.toList
        case _ => Nil // NOTE: other node types have no attributes, but this must NOT evaluate to BOTTOM
      })
    }

    override def getChildren(node: PowersetXMLDomain.N): L = node match {
      case Left(true) => Left(Some(1)) // root nodes have exactly one child
      case Left(false) => Left(None)
      case Right(s) => Right(s.map {
        case XMLRoot(elem) => List(elem)
        case XMLElement(_, _, children, _) => children.toList
        case _ => Nil // NOTE: other node types have no children, but this must NOT evaluate to BOTTOM
      })
    }

    override def getParent(node: N): N = node match {
      case Left(true) => BOT // root nodes don't have a parent
      case Left(false) => Left(false) // we don't know if the parent is a root node
      case Right(s) => Right(s.map(n => n.parent))
    }

    override def concatLists(list1: L, list2: L): L = (list1, list2) match {
      case (BOT_LIST, _) => BOT_LIST
      case (_, BOT_LIST) => BOT_LIST
      case (Right(l1), Right(l2)) => Right(l1.cross(l2).map {
        case (ll1, ll2) => ll1 ++ ll2
      }.toSet)
      case (Right(l1), Left(Some(len2))) => Left(Some(l1.size + len2))
      case (Left(Some(len1)), Right(l2)) => Left(Some(len1 + l2.size))
      case (Left(Some(len1)), Left(Some(len2))) => Left(Some(len1 + len2))
      case _ => Left(None) // at least one operand is TOP and the other is not BOTTOM
    }

    override def partitionAttributes(list: L): (L, L) = list match {
      case Left(_) => (Left(None), Left(None)) // don't know anything about attributes or other nodes
      case Right(s) => val (attr, children) = s.map { l =>
        val resultAttributes = l.takeWhile(n => n.isInstanceOf[XMLAttribute])
        val resultChildren = l.filter(n => !n.isInstanceOf[XMLAttribute])
        (resultAttributes, resultChildren)
      }.unzip
      (Right(attr), Right(children))
    }

    override def wrapInRoot(list: L): N = list match {
      case Left(None) => Left(true)
      case Left(Some(1)) => Left(true)
      case Left(Some(_)) => BOT
      case Right(s) => Right(
        s.filter {
          case List(e: XMLElement) => true
          case l => println(f"[WARNING] Failed to wrap nodes in root: $l"); false
          // NOTE: Lists with more than one node or a non-element node are evaluated to bottom implicitly
        }.map {
          case List(e: XMLElement) => XMLRoot(e)
        }
      )
    }

    override def copyToOutput(list: L): L = list match {
      case Left(_) => list // list with same number of elements (if known)
      case Right(s) => Right(s.map(_.map {
        case XMLRoot(elem) => elem.copy // "a root node is copied by copying its children" according to spec
        case node => node.copy
      }))
    }

    override def getNodeListSize(list: L): V = list match {
      case Left(Some(len)) => xpathDom.liftNumber(len)
      case Left(None) => xpathDom.topNumber
      case Right(s) => xpathDom.join(s.map(l => xpathDom.liftNumber(l.size)).toList)
    }

    override def getStringValue(node: N): V = node match {
      case Left(_) => xpathDom.topString
      case Right(s) => xpathDom.join(s.map(n => xpathDom.liftLiteral(n.stringValue)).toList)
    }

    override def isRoot(node: N): (N, N) = node match {
      case Left(true) => (Left(true), BOT)
      case Left(false) => (Left(true), Left(false)) // the set of all root nodes is a subset of the set of all nodes
      case Right(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLRoot])
        (Right(yes), Right(no))
    }

    override def isElement(node: N): (N, N) = node match {
      case Left(true) => (BOT, Left(true)) // root nodes are not elements
      case Left(false) => (Left(false), Left(false))
      case Right(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLElement])
        (Right(yes), Right(no))
    }

    override def isTextNode(node: N): (N, N) = node match {
      case Left(true) => (BOT, Left(true)) // root nodes are not text nodes
      case Left(false) => (Left(false), Left(false))
      case Right(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLTextNode])
        (Right(yes), Right(no))
    }

    override def isComment(node: N): (N, N) = node match {
      case Left(true) => (BOT, Left(true)) // root nodes are not comments
      case Left(false) => (Left(false), Left(false))
      case Right(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLComment])
        (Right(yes), Right(no))
    }

    override def isAttribute(node: N): (N, N) = node match {
      case Left(true) => (BOT, Left(true)) // root nodes are not attributes
      case Left(false) => (Left(false), Left(false))
      case Right(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLAttribute])
        (Right(yes), Right(no))
    }

    override def hasName(node: N, name: String): (N, N) = node match {
      case Left(true) => (BOT, Left(true)) // root nodes don't have a name
      case Left(false) => (Left(false), Left(false))
      case Right(s) =>
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
        (Right(yes), Right(no))
    }

    override def hasParent(node: N, parent: N): (N, N) = (node, parent) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT) => (BOT, node) // parent is BOTTOM -> can't match
      case (Left(true), _) => (BOT, node) // node is a root node -> can't match
      case (Left(false), _) => (node, node) // don't know anything about the node
      case (Right(_), Left(false)) => (node, node) // parent is TOP -> don't know anything
      case (Right(nodes), Left(true)) => // parent is a root node
        val (yes, no) = nodes.partition(n => n.parent.isInstanceOf[XMLRoot])
        (Right(yes), Right(no))
      case ((Right(nodes), Right(parents))) =>
        val (yes, no) = nodes.partition(n => parents.contains(n.parent))
        (Right(yes), Right(no))
    }

    // return empty string if node has no name
    override def getNodeName(node: N): V = node match {
      case Left(true) => xpathDom.liftLiteral("")
      case Left(false) => xpathDom.topString
      case Right(s) => xpathDom.join(s.map {
        case XMLElement(nodeName, _, _, _) => xpathDom.liftLiteral(nodeName)
        case XMLAttribute(nodeName, _, _) => xpathDom.liftLiteral(nodeName)
        case _ => xpathDom.liftLiteral("")
      }.toList)
    }

    override def getConcatenatedTextNodeValues(list: L): V = list match {
      case Left(_) => xpathDom.topString
      case Right(s) => xpathDom.join(s.map { l =>
        xpathDom.liftLiteral(l.collect { case n: XMLTextNode => n.value }.mkString(""))
      }.toList)
    }

    override def filter(list: L, predicate: N => (N, N)): L = list match {
      case Left(_) => Left(None)
      case Right(s) => Right(s.map(_.filter { n =>
        val node: N = Right(Set(n))
        val (resultTrue, _) = predicate(node)
        // TODO: this could be made more abstract using compare operations (e.g. assert that resultTrue <= node)
        resultTrue match {
          case Left(_) => throw new AssertionError()
          case Right(s) => s.toList match {
            case Nil => false // list without elements -> element was filtered out
            case first :: Nil => true // list with one element -> element was not filtered out
            case _ =>
              // list with more than one element -> this should not happen in this domain
              throw new AssertionError("Filter predicate returned node with more than one possibility.")
          }
        }
      }))
    }

    override def flatMapWithIndex(list: L, f: (N, V) => L): L = list match {
      case Left(_) => Left(None)
      case Right(s) => joinList(s.map { l =>
        val mapped = l.zipWithIndex.map { case (n, i) => f(Right(Set(n)), xpathDom.liftNumber(i)) }
        val flattened = mapped.foldLeft(createEmptyList())((acc, next) => concatLists(acc, next))
        flattened
      }.toList)
    }
  }
}
