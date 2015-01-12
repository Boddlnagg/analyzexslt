package tests

import analysis.domain.powerset.PowersetDomain
import analysis.{AbstractXPathContext, XPathAnalyzer}
import xpath._
import xml.XMLNode

class XPathAbstractEvalSuite extends XPathEvalSuiteBase {
  def eval(expr: String, ctxNode: XMLNode): XPathValue = {
    val analyzer = new XPathAnalyzer(PowersetDomain)
    val result = analyzer.evaluate(XPathParser.parse(expr), AbstractXPathContext(Some(Set(ctxNode)), None, None, Map())).get
    assertResult(1)(result.size)
    result.toList.head
  }
}
