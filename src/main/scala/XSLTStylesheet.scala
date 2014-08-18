import scala.xml._

class XSLTStylesheet(source: Elem) {
  val XSLTNamespace = "http://www.w3.org/1999/XSL/Transform";

  assert(source.namespace == XSLTNamespace, f"Root element must be 'stylesheet' with namespace $XSLTNamespace (a literal result element is not supported as root node)");
  assert(source.attribute("version").get.text == "1.0", "Stylesheet version must be 1.0");
}

abstract class XPathExpression

abstract class XSLTNode
case class LiteralElement(name: String, attributes: Map[String, String] /*use SetAttributeElement for attributes instead?*/, children: Seq[XSLTNode]) extends XSLTNode
case class LiteralTextNode(text: String) extends XSLTNode
case class TemplateElement(name: Option[String], matches: Option[XPathExpression], defaultParams: Map[String, XSLTNode], content: Seq[XSLTNode]) extends XSLTNode
case class VariableDefinitionElement(name: String, content: XSLTNode) extends XSLTNode
case class ApplyTemplatesElement(select: XPathExpression, params: Map[String, XSLTNode]) extends XSLTNode
case class CallTemplateElement(name: String, params: Map[String, XSLTNode]) extends XSLTNode
case class SetAttributeElement(attribute: String, value: XSLTNode)
case class ValueOfElement(select: XPathExpression) extends XSLTNode
case class ChooseElement(branches: Seq[(XPathExpression, XSLTNode)], otherwise: Option[XSLTNode]) extends XSLTNode

