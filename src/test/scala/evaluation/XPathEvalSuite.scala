package evaluation

import xml.XMLNode
import xpath._

class XPathEvalSuite extends XPathEvalSuiteBase {
  def eval(expr: String, ctxNode: XMLNode): XPathValue = {
    XPathProcessor.process(XPathParser.parse(expr), XPathContext(ctxNode, 0, 0, Map()))
  }
}
