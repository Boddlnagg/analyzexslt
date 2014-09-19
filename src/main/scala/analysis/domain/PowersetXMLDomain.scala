package analysis.domain

import xml._
import xslt.{XSLTTemplate, XSLTStylesheet}
import xpath.{XPathMatcher}

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
