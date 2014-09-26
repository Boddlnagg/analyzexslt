package analysis.domain.powerset

import analysis.domain.{XPathDomain, XMLDomain}
import xml._
import xpath._
import xslt.{XSLTStylesheet, XSLTTemplate}

/** Just a wrapper for the type aliases */
object PowersetXMLDomain {
  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets
  type L = Option[Set[List[XMLNode]]]

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    override def top: N = None
    override def bottom: N = Some(Set())

    override def listTop: L = None
    override def listBottom: L = Some(Set())

    override def map(list: L, f: N => N): L = ??? // TODO if needed

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

    override def listJoin(l1: L, l2: L): L = (l1, l2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    override def liftDocument(root: XMLRoot): N = Some(Set(root))

    def liftElement(name: String, attributes: L, children: L): N = {
      // TODO: simplify (remove helper functions that are able to do more than necessary)

      // appendChildren (taking a list of nodes) probably is better than appendChild applied multiple times (because of
      // combinatorial possibilities). We can still lift a single node to a list to append a single child
      def appendChildren(node: N, list: L): N = (node, list) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(n), Some(l)) => Some(n.cross(l).map {
          case (e: XMLElement, ll) => {
            // TODO: do we need to copy the nodes here? (each element of the set must refer to its own copy)
            // TODO: check if there is ever a case where the parent is already set (make sure we always build up result trees from bottom to top)
            assert(e.parent == null)
            val copy = e.copy.asInstanceOf[XMLElement]
            ll.foreach(newChild => {
              assert(newChild.parent == null)
              copy.appendChild(newChild.copy)
            })
            copy
          }
          // TODO: what happens if it's not an XMLElement?
        }.toSet)
      }

      def addAttributes(node: N, list: L): N = (node, list) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(n), Some(l)) => Some(n.cross(l).map {
          case (e: XMLElement, ll) => {
            // TODO: do we need to copy the nodes here? (each element of the set must refer to its own copy)
            // TODO: check if there is ever a case where the parent is already set (make sure we always build up result trees from bottom to top)
            assert(e.parent == null)
            val copy = e.copy.asInstanceOf[XMLElement]
            ll.foreach(newChild => {
              assert(newChild.parent == null)
              copy.addAttribute(newChild.copy.asInstanceOf[XMLAttribute])
            })
            copy
          }
          // TODO: what happens if it's not an XMLElement?
        }.toSet)
      }

      var result: N = Some(Set(XMLElement(name)))
      result = addAttributes(result, attributes)
      result = appendChildren(result, children)
      result
    }

    override def liftList(nodes: List[N]): L = {
      def getProduct(input:List[List[XMLNode]]): List[List[XMLNode]] = input match{
        case Nil => Nil // just in case you input an empty list
        case head::Nil => head.map(_::Nil)
        case head::tail => for(elem<- head; sub <- getProduct(tail)) yield elem::sub
      }

      if (nodes.isEmpty)
        Some(Set(Nil))
      else if (nodes.exists(n => !n.isDefined))
        None
      else
        Some(getProduct(nodes.map(_.get.toList).toList).toSet)
    }

    override def chooseTemplates(sheet: XSLTStylesheet, n: N): Map[XSLTTemplate, N] = n match {
      // don't know anything -> return set of all matchable templates
      case None => sheet.matchableTemplates.map { case (_, tmpl, _, _) => (tmpl, n)}.toMap
      case Some(s) => {
        val result = scala.collection.mutable.Map[XSLTTemplate, N]()
        s.foreach { node =>
          def allMatching = sheet.matchableTemplates.filter { case (path, _, _, _) => XPathMatcher.matches(node, path)}
          if (!allMatching.isEmpty) { // could be empty when builtin templates are disabled
            val (_, template, _, _) = allMatching.last // this one will have highest precedence and priority, because the templates are sorted
            result.get(template) match {
              case None => result.put(template, Some(Set(node)))
              case Some(previous) => result.update(template, join(previous, Some(Set(node))))
            }
          }
        }
        result.toMap
      }
    }

    override def getRoot(node: N): N = node match {
      case None => None // infinite set of all possible roots (in this domain we can't express that it must be a root node)
      case Some(s) => Some(s.map(n => n.root))
    }

    override def getChildren(node: PowersetXMLDomain.N): L = node.map(_.collect {
      case XMLRoot(elem) => List(elem)
      case XMLElement(_, _, children, _) => children.toList
      case _ => Nil // NOTE: other node types have no children
    })

    def getParent(node: N): N = node match {
      case None => None
      case Some(s) => Some(s.map(n => n.parent))
    }

    override def listConcat(list1: L, list2: L): L = (list1, list2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(l1), Some(l2)) => Some(l1.cross(l2).map {
        case (ll1, ll2) => ll1 ++ ll2
      }.toSet)
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
      case Some(s) => Some(s.collect {
        case List(e: XMLElement) => XMLRoot(e)
        // NOTE: Lists with more than one node or a non-element node are evaluated to bottom implicitly
      })
    }

    override def copyToOutput(list: L): L = list.map(_.map(_.map {
      case XMLRoot(elem) => elem.copy // "a root node is copied by copying its children" according to spec
      case node => node.copy
    }))

    override def getNodeListSize(list: L): V = list match {
      case None => xpathDom.top // we could return some "topNumber" or "topInt" here, but we would need to extend our domain to do that
      case Some(s) => xpathDom.join(s.map(l => xpathDom.liftNumber(l.size)).toList)
    }

    override def getStringValue(node: N): V = node match {
      case None => xpathDom.top // we could return some "topString" here, but we would need to extend our domain to do that
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
    override def nameMatches(node: N, name: String): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        // nodes that can't have a name are evaluated to bottom implicitly, i.e. they won't appear in the output at all
        // TODO: is that the right strategy?
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

    // first result is a node of which we KNOW that it matches
    // second result is a node of which we KNOW that it won't match
    def parentMatches(node: N, parent: N): (N, N) = (node, parent) match {
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), None) => (node, node) // don't know anything about the parent
      case ((Some(nodes), Some(parents))) => {
        // TODO: I have the feeling that this is not entirely correct ...
        val yes = nodes.filter(n => parents.contains(n.parent))
        val no = nodes.filter(n => !parents.contains(n.parent))
        (Some(yes), Some(no))
      }
    }

    //override def partitionByParent(node: N, goodParent: N, badParent: N): (N, N) = ???

    //override def parentMatches(node: N, f: N => (N, N)): (N, N) = ???
  }
}
