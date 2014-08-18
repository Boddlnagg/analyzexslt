import scala.xml._

class XSLTStylesheet(var source: Elem) {
  val XSLTNamespace = "http://www.w3.org/1999/XSL/Transform";

  val trimmed = xml.Utility.trim(source).asInstanceOf[Elem]

  assert(trimmed.namespace == XSLTNamespace, f"Root element must be 'stylesheet' with namespace $XSLTNamespace (a literal result element is not supported as root node)");
  assert(trimmed.attribute("version").get.text == "1.0", "Stylesheet version must be 1.0");
  assert(trimmed.child.forall(n => n.namespace == XSLTNamespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions");

  // TODO: the spec requires us to evaluate variables in an order such that variables can depend on each other (as long
  // as there are no circular dependencies, which would result in an error), see spec section 11.4
  // var topLevelVariables = ...
  var templates = trimmed.child
    .filter(n => n.isInstanceOf[Elem] && n.label == "template")
    .map(n => n.asInstanceOf[Elem])
    .map(elem => TemplateElement(
                  elem.attribute("name").map(_.text),
                  elem.attribute("match").map(a => XPathExpression(a.text)),
                  parseParams(elem.child),
                  parseTemplate(elem.child.filter(n => !(n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "param")))
                 ));

  def parseTemplate(template: Seq[Node]) : Seq[XSLTNode] = {
    // filter out comments first
    template.filter(n => !n.isInstanceOf[Comment]).map(parseNode(_));
  }

  def parseNode(node: Node) : XSLTNode = {
    if (node.isInstanceOf[Text]) {
      LiteralTextNode(node.asInstanceOf[Text].data)
    } else if (node.isInstanceOf[Elem]) {
      if (node.namespace == XSLTNamespace) {
        node.label match {
          case "variable" => throw new UnsupportedOperationException("TODO") // TODO
          case "apply-templates" => throw new UnsupportedOperationException("TODO") // TODO
          case "call-template" => throw new UnsupportedOperationException("TODO") // TODO
          case "attribute" => throw new UnsupportedOperationException("TODO") // TODO
          case "value-of" => throw new UnsupportedOperationException("TODO") // TODO
          case "choose" => throw new UnsupportedOperationException("TODO") // TODO
          case "if" => throw new UnsupportedOperationException("TODO") // TODO
        }
      } else if (node.namespace == null) {
        LiteralElement(node.label,
          node.attributes.asAttrMap, // TODO: use SetAttributeElement for attributes instead (to simplify representation)?
          parseTemplate(node.child)
        )
      } else {
        throw new UnsupportedOperationException("Namespaces other than the XSLT namespace are not supported.")
      }
    } else {
      throw new UnsupportedOperationException(f"Unsupported XML node $node")
    }
  }

  def parseParams(input: Seq[Node]) : Map[String, Seq[XSLTNode]] = {
    // TODO: support "select" attribute on params
    val params = input.filter(n => n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "param")
                      .map(n => n.asInstanceOf[Elem])
                      .map(elem => (elem.attribute("name").get.text, parseTemplate(elem.child)))
    Map() ++ params
  }
}

case class TemplateElement(name: Option[String],
                           matches: Option[XPathExpression],
                           defaultParams: Map[String, Seq[XSLTNode]],
                           content: Seq[XSLTNode])

// TODO: parse XPath
case class XPathExpression(expression: String)

abstract class XSLTNode
case class LiteralElement(name: String, attributes: Map[String, String], children: Seq[XSLTNode]) extends XSLTNode
case class LiteralTextNode(text: String) extends XSLTNode
case class VariableDefinitionElement(name: String, content: XSLTNode) extends XSLTNode
case class ApplyTemplatesElement(select: XPathExpression, params: Map[String, XSLTNode]) extends XSLTNode
case class CallTemplateElement(name: String, params: Map[String, XSLTNode]) extends XSLTNode
case class SetAttributeElement(attribute: String, value: XSLTNode)
case class ValueOfElement(select: XPathExpression) extends XSLTNode
case class ChooseElement(branches: Seq[(XPathExpression, XSLTNode)], otherwise: Option[XSLTNode]) extends XSLTNode

