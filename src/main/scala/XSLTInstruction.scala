/** Base class for XSLT instructions */
abstract class XSLTInstruction

/** Literal result elements (see XSLT spec section 7.1.1)
  *
  * @param name the name of the element (namespaces are not implemented)
  * @param attributes the set of attributes for the element (TODO: maybe use set attribute instruction children instead?)
  * @param children the instructions used to generate the children of the element
  */
case class LiteralElement(name: String, attributes: Map[String, String], children: Seq[XSLTInstruction]) extends XSLTInstruction

/** Literal text nodes (see XSLT spec section 7.2)
  *
  * @param text the text of the text node
  */
case class LiteralTextNode(text: String) extends XSLTInstruction

/** Variable definitions, created using the &lt;xsl:variable&gt; element (see XSLT spec section 11)
  *
  * @param name the name of the variable
  * @param select the XPath expression used to compute the value
  */
case class VariableDefinitionInstruction(name: String, select: XPathExpr) extends XSLTInstruction

/** Apply templates instruction, created using the &lt;xsl:apply-templates&gt; element (see XSLT spec 5.4)
  *
  * @param select an optional XPath expression (must return a node-set) to select only a subset of the child nodes
  * @param params the parameters for the template instantiation, as defined with &lt;xsl:with-param&gt;
  */
case class ApplyTemplatesInstruction(select: Option[XPathExpr] = None, params: Map[String, XPathExpr] = Map()) extends XSLTInstruction

/** Call template instruction, created using the &lt;xsl:call-template&gt; element (see XSLT spec section 6)
  *
  * @param name the name of the called template
  * @param params the parameters for the template instantiation, as defined with &lt;xsl:with-param&gt;
  */
case class CallTemplatesInstruction(name: String, params: Map[String, XPathExpr] = Map()) extends XSLTInstruction

/** Set attribute instruction, created using the &lt;xsl:attribute&gt; element (see XSLT spec section 7.1.3)
  *
  * @param attribute the name of the attribute
  * @param value the instructions used to compute the value of the attribute
  */
case class SetAttributeInstruction(attribute: String, value: Seq[XSLTInstruction]) extends XSLTInstruction

/** Copy instruction, created using the &lt;xsl:copy-of&gt; or &lt;xsl:value-of&gt; elements (see XSLT spec sections 7.6.1 and 11.3)
  * For &lt;xsl:value-of&gt;, the result will be converted to a string by wrapping the select expression with a call to the string() function.
  *
  * @param select The XPath expression to compute the value that is copied (either a node-set or any other node
  *               which is then first converted to a string and results in a text node)
  */
case class CopyInstruction(select: XPathExpr) extends XSLTInstruction

/** Choose instruction, created using the &lt;xsl:choose&gt; or the &lt;xsl:if&gt; elements (see XSLT spec sections 9.1 and 9.2)
  *
  * @param branches a list of branches, where each branch consists of a test-expression and a sequence of instructions
  * @param otherwise the instructions that are evaluated when no previous branch matches
  */
case class ChooseInstruction(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction]) extends XSLTInstruction