package xslt

import xpath.XPathExpr

/** An XSLT template
  *
  * @param content the content of the template, represented as a sequence of XSLT instructions
  * @param defaultParams the parameters of this template along with expressions to compute the default values (these must be evaluated lazily)
  */
class XSLTTemplate(val content: Seq[XSLTInstruction], val defaultParams: Map[String, XPathExpr] = Map())