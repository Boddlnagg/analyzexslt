import scala.xml.{Text, Node, Elem}

class XSLTTemplate(val name: Option[String],
                   val matches: Option[XPathExpr],
                   val defaultParams: Map[String, XPathExpr],
                   val content: Seq[XSLTNode]) {

  if (matches.isDefined) assert(XPathExpr.isPattern(matches.get), "Template 'match' attribute must be a pattern.")

  def priority = ??? // TODO (spec section 5.5)
}

object XSLTTemplate {
  def apply(elem: Elem): XSLTTemplate = {
    assert(XSLT.isElem(elem, "template"))
    new XSLTTemplate(
      elem.attribute("name").map(_.text),
      elem.attribute("match").map(a => XPathExpr(a.text)),
      parseParams(elem.child),
      parseTemplate(elem.child.filter(n => !XSLT.isElem(n, "param")))
    )
  }

  def parseTemplate(template: Seq[Node]) : Seq[XSLTNode] = {
    template.map(parseNode).toList
  }

  def parseNode(node: Node) : XSLTNode = node match {
    case text : Text => LiteralTextNode(text.data)
    case elem : Elem => elem.namespace match {
      case XSLT.Namespace => elem.label match {
        // spec section 11.2
        case "variable" =>
          assert(elem.child.nonEmpty, "Variable definitions are only supported when they use the 'select' attribute")
          val select = XPathExpr(elem.attribute("select").get.text)
          VariableDefinitionElement(elem.attribute("name").get.text, select)

        // spec sections 5.4 and 11.6
        case "apply-templates" =>
          val select = elem.attribute("select").map(a => XPathExpr(a.text))
          assert(elem.child.forall(XSLT.isElem(_, "with-param")),
            "children of 'apply-templates' element must only be 'with-param' ('sort' is not supported)")
          ApplyTemplatesElement(select, parseWithParams(elem))

        // spec section 6
        case "call-template" =>
          val name = elem.attribute("name").get.text
          assert(elem.child.forall(XSLT.isElem(_, "with-param")),
            "children of 'call-templates' element must only be 'with-param'")
          CallTemplateElement(name, parseWithParams(elem))

        // spec section 7.1.3
        case "attribute" =>
          // NOTE: attribute value templates are not supported
          val name = elem.attribute("name").get.text
          assert(!elem.attribute("namespace").isDefined, "The 'namespace' attribute on xsl:attribute is not supported.")
          SetAttributeElement(name, parseTemplate(elem.child)) // NOTE: only text nodes are allowed in the instantiation of this template

        // spec section 7.6.1
        case "value-of" =>
          ValueOfElement(XPathExpr(elem.attribute("select").get.text))

        // spec section 9.2
        case "choose" =>
          val xsltChildren = elem.child.filter(XSLT.isElem).map(_.asInstanceOf[Elem])
          val whenBranches = xsltChildren.filter(n => n.label == "when")
            .map(n => (XPathExpr(n.attribute("test").get.text), parseTemplate(n.child)))
          val otherwiseBranch = xsltChildren.filter(n => n.label == "otherwise")
            .map(n => parseTemplate(n.child))
            .headOption
          ChooseElement(whenBranches.toList, otherwiseBranch)

        // spec section 9.1
        case "if" =>
          val test = XPathExpr(elem.attribute("test").get.text)
          ChooseElement(List((test, parseTemplate(elem.child))), None)

        // spec section 7.2 and 3.4 (whitespace stripping)
        case "text" =>
          LiteralTextNode(elem.text)

        case _ => throw new UnsupportedOperationException(f"Unsupported XSLT element: ${elem.label}")
      }
      case null | "" =>
        // element without namespace
        LiteralElement(node.label,
          node.attributes.asAttrMap, // TODO: use SetAttributeElement for attributes instead (to simplify representation)?
          parseTemplate(node.child)
        )
      case _ => throw new UnsupportedOperationException("Namespaces other than the XSLT namespace are not supported.")
    }
    case _ => throw new UnsupportedOperationException(f"Unsupported XML node $node")
  }

  def parseParams(input: Seq[Node]) : Map[String, XPathExpr] = {
    // TODO: support content of param element instead of "select" attribute?
    val params = input.filter(XSLT.isElem(_, "param"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathExpr(elem.attribute("select").get.text)))
    Map() ++ params
  }

  def parseWithParams(input: Seq[Node]) : Map[String, XPathExpr] = {
    // TODO: support content of with-param element instead of "select" attribute?
    // TODO: merge function with parseParams() above
    val params = input.filter(n => n.isInstanceOf[Elem] && n.namespace == XSLT.Namespace && n.label == "with-param")
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathExpr(elem.attribute("select").get.text)))
    Map() ++ params
  }
}
