package analysis.domain.powerset

import analysis.domain.{XPathDomain, XMLDomain}
import xml._

/** Just a wrapper for the type aliases */
object PowersetXMLDomain {
  // TODO: use L = Option[Set[List[XMLNode]]] again and modify implementation to use Lattice type class

  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets
  type L = Either[Option[Int], Set[List[XMLNode]]] // Left(Some(n)) is a list with n unknown elements (n > 0)

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    override def top: N = None
    override def bottom: N = BOT

    protected val BOT: N = Some(Set())

    override def topList: L = Left(None)
    override def bottomList: L = BOT_LIST

    protected val BOT_LIST: L = Right(Set())

    override def join(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    override def meet(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => n2
      case (_, None) => n1
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }

    override def lessThanOrEqual(n1: N, n2: N): Boolean = (n1, n2) match {
      case (_, None) => true
      case (None, Some(_)) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }

    override def lessThanOrEqualLists(l1: L, l2: L): Boolean = (l1, l2) match {
      case (_, Left(None)) => true
      case (Left(Some(len1)), Left(Some(len2))) => len1 == len2
      case (Left(None), Left(Some(_))) => false
      case (Right(_), Left(_)) => true
      case (Left(_), Right(_)) => false
      case (Right(s1), Right(s2)) => s1.subsetOf(s2)
    }

    override def joinLists(l1: L, l2: L): L = (l1, l2) match {
      case (Left(Some(len1)), Left(Some(len2))) if len1 == len2 => l1
      case (Left(_), Left(_)) => Left(None)
      case (Left(Some(len1)), Right(s2)) if len1 == s2.size => Left(Some(len1))
      case (Left(_), Right(_)) => Left(None)
      case (Right(s1), Left(Some(len2))) if s1.size == len2 => Left(Some(len2))
      case (Right(_), Left(_)) => Left(None)
      case (Right(s1), Right(s2)) => Right(s1.union(s2))
    }

    override def createElement(name: String, attributes: L, children: L): N = (attributes, children) match {
      case (Left(_), _) => None
      case (_, Left(_)) => None
      case (Right(s1), Right(s2)) => Some(s1.cross(s2).map {
        case (attr, chld) => XMLElement(name,
          attr.map(a => a.asInstanceOf[XMLAttribute].copy),
          chld.map(c => c.copy))
      }.toSet)
    }

    override def createEmptyList(): L = Right(Set(Nil))

    override def createSingletonList(node: N): L = node match {
      case None => Left(Some(1))
      case Some(s) => Right(s.map(n => List(n)))
    }

    override def getRoot(node: N): N = node match {
      case None => None // infinite set of all possible roots (in this domain we can't express that it must be a root node)
      case Some(s) => Some(s.map(n => n.root))
    }

    override def getAttributes(node: N): L = node match {
      case None => Left(None)
      case Some(s) => Right(s.map {
        case XMLElement(_, attr, _, _) => attr.toList
        case _ => Nil // NOTE: other node types have no attributes, but this must NOT evaluate to BOTTOM
      })
    }

    override def getChildren(node: PowersetXMLDomain.N): L = node match {
      case None => Left(None)
      case Some(s) => Right(s.map {
        case XMLRoot(inner) => List(inner)
        case XMLElement(_, _, children, _) => children.toList
        case _ => Nil // NOTE: other node types have no children, but this must NOT evaluate to BOTTOM
      })
    }

    override def getParent(node: N): N = node match {
      case None => None
      case Some(s) => Some(s.collect {
        case e if !e.isInstanceOf[XMLRoot] =>
          assert(e.parent != null)
          e.parent
      })
    }

    override def concatLists(list1: L, list2: L): L = (list1, list2) match {
      case (BOT_LIST, _) => BOT_LIST
      case (_, BOT_LIST) => BOT_LIST
      case (Right(l1), Right(l2)) => Right(l1.cross(l2).map {
        case (ll1, ll2) => ll1 ++ ll2
      }.toSet)
      case (Right(s1), Left(Some(len2))) =>
        val sizes = s1.map(_.size).toSet
        if (sizes.size == 1) Left(Some(sizes.toList.head + len2))
        else Left(None)
      case (Left(Some(len1)), Right(s2)) =>
        val sizes = s2.map(_.size).toSet
        if (sizes.size == 1) Left(Some(sizes.toList.head + len1))
        else Left(None)
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
      case Left(None) => None
      case Left(Some(1)) => None
      case Left(Some(_)) => BOT
      case Right(s) => Some(
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
        case XMLRoot(inner) => inner.copy // "a root node is copied by copying its children" according to spec
        case node => node.copy
      }))
    }

    override def getNodeListSize(list: L): V = list match {
      case Left(Some(len)) => xpathDom.liftNumber(len)
      case Left(None) => xpathDom.topNumber
      case Right(s) => xpathDom.joinAll(s.map(l => xpathDom.liftNumber(l.size)))
    }

    override def getStringValue(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.joinAll(s.map(n => xpathDom.liftString(n.stringValue)))
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

    override def isContainedIn(node: N, list: L): (N, N) = (node, list) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT_LIST) => (BOT, node) // list is BOTTOM -> can't be contained in it
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), Left(_)) => (node, node) //  list is TOP (or has unknown elements) -> don't know if it contains the node
      case (Some(nodes), Right(lists)) =>
        val (yes, no) = nodes.partition(n => lists.exists(l => l.contains(n)))
        (Some(yes), Some(no))
    }

    // return empty string if node has no name
    override def getNodeName(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.joinAll(s.map {
        case XMLElement(nodeName, _, _, _) => xpathDom.liftString(nodeName)
        case XMLAttribute(nodeName, _, _) => xpathDom.liftString(nodeName)
        case _ => xpathDom.liftString("")
      })
    }

    override def getConcatenatedTextNodeValues(list: L): V = list match {
      case Left(_) => xpathDom.topString
      case Right(s) => xpathDom.joinAll(s.map { l =>
        xpathDom.liftString(l.collect { case n: XMLTextNode => n.value }.mkString(""))
      })
    }

    override def filter(list: L, predicate: N => (N, N)): L = list match {
      case Left(_) => Left(None)
      case Right(s) => Right(s.map(_.filter { n =>
        val node: N = Some(Set(n))
        val (resultTrue, _) = predicate(node)
        assert(lessThanOrEqual(resultTrue, node))
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
      case Left(None) => // list with unknown number of unknown elements
        val mappedNone = f(None, xpathDom.topNumber) // call f with TOP element and TOP index
        mappedNone match {
          case Right(s) => s.toList match {
            case Nil => Right(Set()) // f always returns BOTTOM -> return BOTTOM
            case List(Nil) => Right(Set(Nil)) // f always returns an empty list -> return empty list
            case _ => Left(None) // f returns some elements
          }
          case Left(_) => Left(None) // don't result elements -> don't know output
        }
      case Left(Some(len)) =>
        val mapped = List.range(0, len).map { i => f(None, xpathDom.liftNumber(i)) }
        val flattened = mapped.foldLeft(createEmptyList())((acc, next) => concatLists(acc, next))
        flattened
      case Right(s) => joinAllLists(s.map { l =>
        val mapped = l.zipWithIndex.map { case (n, i) => f(Some(Set(n)), xpathDom.liftNumber(i)) }
        val flattened = mapped.foldLeft(createEmptyList())((acc, next) => concatLists(acc, next))
        flattened
      })
    }

    override def getFirst(list: L): N = list match {
      case Left(None) => None
      case Left(Some(len)) => None // Left(Some(_)) always describes a non-empty list, so we can return TOP
      case Right(s) => Some(s.collect {
        case head :: _ => head
      })
    }
  }
}
