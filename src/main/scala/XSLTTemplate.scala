import scala.xml.{Text, Node, Elem}

/** An XSLT template
  *
  * @param content the content of the template, represented as a sequence of XSLT instructions
  * @param defaultParams the parameters of this template along with expressions to compute the default values (these must be evaluated lazily)
  */
class XSLTTemplate(val content: Seq[XSLTInstruction], val defaultParams: Map[String, XPathExpr] = Map())

/** Factory for [[XSLTTemplate]] instances */
object XSLTTemplate {
  /** Creates a new XSLT template from a [[scala.xml.Elem]] (must be an &lt;xsl:template&gt; element) */
  def apply(elem: Elem): XSLTTemplate = {
    assert(XSLT.isElem(elem, "template"))
    new XSLTTemplate(
      parseTemplate(elem.child.filter(n => !XSLT.isElem(n, "param"))),
      parseParams(elem.child)
    )
  }

  /** Parses a sequence of [[scala.xml.Node]]s as XSLT instructions */
  def parseTemplate(template: Seq[Node]) : Seq[XSLTInstruction] = {
    template.map(parseInstruction).toList
  }

  /** Parses a single [[scala.xml.Node]] as an XSLT instruction */
  def parseInstruction(node: Node) : XSLTInstruction = node match {
    case text : Text => LiteralTextNode(text.data)
    case elem : Elem => elem.namespace match {
      case XSLT.Namespace => elem.label match {
        // spec section 11.2
        case "variable" =>
          assert(elem.child.isEmpty, "Variable definitions are only supported when they use the 'select' attribute")
          // value is empty string '' if there is no select attribute (see XSLT spec section 11.2)
          val select = XPathExpr(elem.attribute("select").map(_.text).getOrElse("''"))
          VariableDefinitionInstruction(elem.attribute("name").get.text, select)

        // spec sections 5.4 and 11.6
        case "apply-templates" =>
          val select = elem.attribute("select").map(a => XPathExpr(a.text))
          assert(elem.child.forall(XSLT.isElem(_, "with-param")),
            "children of 'apply-templates' element must only be 'with-param' ('sort' is not supported)")
          ApplyTemplatesInstruction(select, parseWithParams(elem.child))

        // spec section 6
        case "call-template" =>
          val name = elem.attribute("name").get.text
          assert(elem.child.forall(XSLT.isElem(_, "with-param")),
            "children of 'call-templates' element must only be 'with-param'")
          CallTemplatesInstruction(name, parseWithParams(elem.child))

        // spec section 7.1.3
        case "attribute" =>
          // NOTE: attribute value templates are not supported
          val name = elem.attribute("name").get.text
          assert(!elem.attribute("namespace").isDefined, "The 'namespace' attribute on xsl:attribute is not supported.")
          SetAttributeInstruction(name, parseTemplate(elem.child)) // NOTE: only text nodes are allowed in the instantiation of this template

        // spec section 7.6.1
        case "value-of" =>
          // <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
          CopyInstruction(XPathExpr("string(" + elem.attribute("select").get.text +")"))

        // spec section 11.3
        case "copy-of" =>
          CopyInstruction(XPathExpr(elem.attribute("select").get.text))

        // spec section 9.2
        case "choose" =>
          val xsltChildren = elem.child.filter(XSLT.isElem).map(_.asInstanceOf[Elem])
          val whenBranches = xsltChildren.filter(n => n.label == "when")
            .map(n => (XPathExpr(n.attribute("test").get.text), parseTemplate(n.child)))
          val otherwiseBranch = xsltChildren.filter(n => n.label == "otherwise")
            .map(n => parseTemplate(n.child))
            .headOption.getOrElse(Seq())
          ChooseInstruction(whenBranches.toList, otherwiseBranch)

        // spec section 9.1
        case "if" =>
          val test = XPathExpr(elem.attribute("test").get.text)
          ChooseInstruction(List((test, parseTemplate(elem.child))), Seq())

        // spec section 7.2 and 3.4 (whitespace stripping)
        case "text" =>
          LiteralTextNode(elem.text)

        case _ => throw new NotImplementedError(f"Unsupported XSLT element: ${elem.label}")
      }
      case null | "" =>
        // element without namespace
        LiteralElement(node.label,
          node.attributes.asAttrMap, // TODO: use SetAttributeElement for attributes instead (to simplify representation)?
          parseTemplate(node.child)
        )
      case _ => throw new NotImplementedError("Namespaces other than the XSLT namespace are not supported.")
    }
    case _ => throw new NotImplementedError(f"Unsupported XML node $node")
  }

  /** Parses &lt;xsl:param&gt; nodes */
  def parseParams(input: Seq[Node]) : Map[String, XPathExpr] = {
    // TODO: support content of param element instead of "select" attribute?
    val params = input.filter(XSLT.isElem(_, "param"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathExpr(elem.attribute("select").get.text)))
    Map() ++ params
  }

  /** Parses &lt;xsl:with-param&gt; nodes */
  def parseWithParams(input: Seq[Node]) : Map[String, XPathExpr] = {
    // TODO: support content of with-param element instead of "select" attribute?
    // TODO: merge function with parseParams() above
    val params = input.filter(XSLT.isElem(_, "with-param"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathExpr(elem.attribute("select").get.text)))
    Map() ++ params
  }
}
