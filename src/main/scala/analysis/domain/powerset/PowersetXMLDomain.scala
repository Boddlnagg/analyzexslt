package analysis.domain.powerset

import analysis.domain.XMLDomain
import xml._
import xpath._
import xslt.{XSLTStylesheet, XSLTTemplate}

object PowersetXMLDomain {
  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets
  type L = Option[Set[List[N]]]

  object D extends XMLDomain[N, L] {
    override def top: N = None
    override def bottom: N = Some(Set())

    override def listTop: L = None
    override def listBottom: L = Some(Set())

    override def lift(n: XMLNode): N = Some(Set(n))

    override def liftToList(n: N): L = Some(Set(List(n)))

    override def compare(morePrecise: N, lessPrecise: N): Boolean = (morePrecise, lessPrecise) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }

    override def chooseTemplates(sheet: XSLTStylesheet, n: N): Set[XSLTTemplate] = n match {
      // don't know anything -> return set of all matchable templates
      case None => sheet.matchableTemplates.map { case (_, tmpl, _, _) => tmpl}.toSet
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
