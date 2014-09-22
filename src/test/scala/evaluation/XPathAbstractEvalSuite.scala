package evaluation

import analysis.domain.powerset.{PowersetXPathXMLDomain, PowersetXPathDomain, PowersetXMLDomain}
import analysis.{AbstractXPathContext, XPathAnalyzer}
import xpath._

class XPathAbstractEvalSuite extends XPathEvalSuiteBase {
  object XPathTestAnalyzer extends XPathAnalyzer[PowersetXMLDomain.N, PowersetXMLDomain.D.type, PowersetXPathDomain.T, PowersetXPathXMLDomain.type] {
    val dom1 = PowersetXMLDomain.D
    val dom2 = PowersetXPathXMLDomain
  }

  def eval(expr: String): XPathValue = {
    // evaluate with an empty context
    val result = XPathTestAnalyzer.evaluate(XPathParser.parse(expr), AbstractXPathContext(null, Some(0), Some(0), Map())).get
    assertResult(1)(result.size)
    result.toList.head
  }
}
