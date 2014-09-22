package evaluation

import xml.XMLNode
import xpath._

class XPathEvalSuite extends XPathEvalSuiteBase {
  def eval(expr: String, ctxNode: XMLNode): XPathValue = {
    // evaluate with an empty context
    XPathEvaluator.evaluate(XPathParser.parse(expr), XPathContext(ctxNode, 0, 0, Map()))
  }
}
