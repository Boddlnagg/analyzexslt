package evaluation

import xpath._

class XPathEvalSuite extends XPathEvalSuiteBase {
  def eval(expr: String): XPathValue = {
    // evaluate with an empty context
    XPathEvaluator.evaluate(XPathParser.parse(expr), XPathContext(null, 0, 0, Map()))
  }
}
