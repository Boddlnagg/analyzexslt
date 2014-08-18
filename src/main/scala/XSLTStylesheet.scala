import scala.xml._

class XSLTStylesheet(var source: Elem) {
  val XSLTNamespace = "http://www.w3.org/1999/XSL/Transform";

  // the content of xsl:text should not be trimmed, but is currently not supported anyway
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
      val elem = node.asInstanceOf[Elem]
      if (elem.namespace == XSLTNamespace) {
        elem.label match {
          // spec section 11.2
          case "variable" => {
            assert(elem.child.nonEmpty, "Variable definitions are only supported when they use the 'select' attribute")
            val select = XPathExpression(elem.attribute("select").get.text)
            VariableDefinitionElement(elem.attribute("name").get.text, select)
          }
          // spec sections 5.4 and 11.6
          case "apply-templates" => {
            val select = elem.attribute("select").map(a => XPathExpression(a.text))
            assert(elem.child.forall(n => n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "with-param"),
              "children of 'apply-templates' element must only be 'with-param' ('sort' is not supported)")
            ApplyTemplatesElement(select, parseWithParams(elem))
          }
          // spec section 6
          case "call-template" => {
            val name = elem.attribute("name").get.text
            assert(elem.child.forall(n => n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "with-param"),
              "children of 'call-templates' element must only be 'with-param'")
            CallTemplateElement(name, parseWithParams(elem))
          }
          // spec section 7.1.3
          case "attribute" => {
            // NOTE: attribute value templates are not supported
            val name = elem.attribute("name").get.text
            assert(!elem.attribute("namespace").isDefined, "The 'namespace' attribute on xsl:attribute is not supported.")
            SetAttributeElement(name, parseTemplate(elem.child)) // NOTE: only text nodes are allowed in the instantiation of this template
          }
          // spec section 7.6.1
          case "value-of" => {
            ValueOfElement(XPathExpression(elem.attribute("select").get.text))
          }
          // spec section 9.2
          case "choose" => {
            val xsltChildren = elem.child.filter(n => n.namespace == XSLTNamespace && n.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
            val whenBranches = xsltChildren.filter(n => n.label == "when")
                                           .map(n => (XPathExpression(n.attribute("test").get.text), parseTemplate(n.child)))
            val otherwiseBranch = xsltChildren.filter(n => n.label == "otherwise")
                                              .map(n => parseTemplate(n.child))
                                              .headOption
            ChooseElement(whenBranches, otherwiseBranch)
          }
          // spec section 9.1
          case "if" => {
            val test = XPathExpression(elem.attribute("test").get.text)
            ChooseElement(List((test, parseTemplate(elem.child))), None)
          }
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

  def parseParams(input: Seq[Node]) : Map[String, XPathExpression] = {
    // TODO: support content of param element instead of "select" attribute?
    val params = input.filter(n => n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "param")
                      .map(n => n.asInstanceOf[Elem])
                      .map(elem => (elem.attribute("name").get.text, XPathExpression(elem.attribute("select").get.text)))
    Map() ++ params
  }

  def parseWithParams(input: Seq[Node]) : Map[String, XPathExpression] = {
    // TODO: support content of with-param element instead of "select" attribute?
    // TODO: merge function with parseParams() above
    val params = input.filter(n => n.isInstanceOf[Elem] && n.namespace == XSLTNamespace && n.label == "with-param")
                      .map(n => n.asInstanceOf[Elem])
                      .map(elem => (elem.attribute("name").get.text, XPathExpression(elem.attribute("select").get.text)))
    Map() ++ params
  }
}

case class TemplateElement(name: Option[String],
                           matches: Option[XPathExpression],
                           defaultParams: Map[String, XPathExpression],
                           content: Seq[XSLTNode])

// TODO: parse XPath
case class XPathExpression(expression: String)

abstract class XSLTNode
case class LiteralElement(name: String, attributes: Map[String, String], children: Seq[XSLTNode]) extends XSLTNode
case class LiteralTextNode(text: String) extends XSLTNode
case class VariableDefinitionElement(name: String, select: XPathExpression) extends XSLTNode
case class ApplyTemplatesElement(select: Option[XPathExpression], params: Map[String, XPathExpression]) extends XSLTNode
case class CallTemplateElement(name: String, params: Map[String, XPathExpression]) extends XSLTNode
case class SetAttributeElement(attribute: String, value: Seq[XSLTNode]) extends XSLTNode
case class ValueOfElement(select: XPathExpression) extends XSLTNode
case class ChooseElement(branches: Seq[(XPathExpression, Seq[XSLTNode])], otherwise: Option[Seq[XSLTNode]]) extends XSLTNode

