package evaluation

import analysis.domain.powerset.{PowersetXPathXMLDomain, PowersetXPathDomain, PowersetXMLDomain}
import analysis.{AbstractXPathContext, XPathAnalyzer}
import xpath._
import xml.XMLNode

class XPathAbstractEvalSuite extends XPathEvalSuiteBase {
  object XPathTestAnalyzer extends XPathAnalyzer[PowersetXMLDomain.N, PowersetXMLDomain.L, PowersetXMLDomain.D.type, PowersetXPathDomain.T, PowersetXPathXMLDomain.type] {
    val dom1 = PowersetXMLDomain.D
    val dom2 = PowersetXPathXMLDomain
  }

  def eval(expr: String, ctxNode: XMLNode): XPathValue = {
    // evaluate with an empty context
    val result = XPathTestAnalyzer.evaluate(XPathParser.parse(expr), AbstractXPathContext(PowersetXMLDomain.D.lift(ctxNode), None, None, Map())).get
    assertResult(1)(result.size)
    result.toList.head
  }
}
