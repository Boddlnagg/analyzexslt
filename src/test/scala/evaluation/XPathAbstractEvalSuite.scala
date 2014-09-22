package evaluation

import analysis.domain.{PowersetXMLDomain, PowersetXPathDomain}
import analysis.{AbstractXPathContext, XPathAnalyzer}
import xpath._

class XPathAbstractEvalSuite extends XPathEvalSuiteBase {

  object PowersetXPathXMLDomain extends PowersetXPathDomain[PowersetXMLDomain.N, PowersetXMLDomain.D.type]

  object XPathTestAnalyzer extends XPathAnalyzer[PowersetXMLDomain.N, PowersetXMLDomain.D.type, PowersetXPathXMLDomain.T, PowersetXPathXMLDomain.D.type] {
    val dom1 = PowersetXMLDomain.D
    val dom2 = PowersetXPathXMLDomain.D
  }

  def eval(expr: String): XPathValue = {
    // evaluate with an empty context
    val result = XPathTestAnalyzer.evaluate(XPathParser.parse(expr), AbstractXPathContext(null, Some(0), Some(0), Map())).get
    assertResult(1)(result.size)
    result.toList.head
  }
}
