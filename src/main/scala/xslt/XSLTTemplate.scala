package xslt

import xpath.XPathExpr

/** An XSLT template, consisting of a sequence of XSLT instructions and a number of default parameter values.
  *
  * @param content the content of the template, represented as a sequence of XSLT instructions
  * @param defaultParams the parameters of this template along with expressions to compute the default values (these must be evaluated lazily)
  */
case class XSLTTemplate(content: Seq[XSLTInstruction], defaultParams: Map[String, XPathExpr] = Map())