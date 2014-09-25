package xslt

import xpath._

import scala.xml.{Comment, Elem, Node, Text}

object XSLTParser {
  /** The XSLT namespace */
  val Namespace = "http://www.w3.org/1999/XSL/Transform"

  /** Returns a value indicating whether the given node is an element node and has the XSLT namespace */
  def isElem(node: Node): Boolean = {
    node.isInstanceOf[Elem] && node.namespace == Namespace
  }

  /** Returns a value indicating whether the given node is an element node with the specified name and has the XSLT namespace */
  def isElem(node: Node, name: String): Boolean = {
    isElem(node) && node.label == name
  }

  /** Preprocesses a [[scala.xml.Node]] by removing whitespace-only text-nodes (except inside &lt;xslt:text&gt;) and comment nodes. */
  def clean(x: Node): Node = x match {
    // This implementation is based on scala.xml.Utility.trim
    case Elem(pre, lab, md, scp, child@_*) =>
      val children = child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
  }

  def cleanProper(x: Node): Seq[Node] = x match {
    case Elem(pre, lab, md, scp, child@_*) =>
      // preserve whitespace inside <xsl:text> (see XSLT spec section 16.1)
      val children = if (isElem(x, "text")) child else child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
    case Text(s) =>
      if (isOnlyWhitespace(s)) Nil else List(Text(s))
    case Comment(_) => Seq.empty // strip comments completely (see XSLT spec section 3)
    case _ =>
      x
  }

  /** Returns a value indicating whether the given string consists of only whitespace characters */
  def isOnlyWhitespace(s: String) = {
    val xmlWhitespace = List('\t', '\n', '\r', ' ') // according to XSLT spec section 3.4
    s.forall(c => xmlWhitespace.contains(c))
  }

  def parseStylesheet(source: Elem): XSLTStylesheet = {
    val cleaned = clean(source).asInstanceOf[Elem]

    if (cleaned.namespace != Namespace) throw new NotImplementedError(f"Root element must be 'stylesheet' with namespace $Namespace (a literal result element is not supported as root node)")
    if (cleaned.attribute("version").get.text != "1.0") throw new NotImplementedError("Stylesheet versions other than 1.0 are not supported")
    assert(cleaned.child.forall(n => n.namespace == Namespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions")

    // TODO: the spec requires us to evaluate top-level variables in an order such that variables can depend on each other
    // (as long as there are no circular dependencies, which would result in an error), see spec section 11.4
    // -> don't support top-level variables?
    val globalVariables = cleaned.child
      .filter(isElem(_, "variable"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathParser.parse(elem.attribute("select").map(_.text).getOrElse("''"))))

    if (!globalVariables.isEmpty) throw new NotImplementedError("Top-level variables are not implemented.")

    val templates = cleaned.child
      .filter(isElem(_, "template"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (parseTemplate(elem),
        elem.attribute("name").map(_.text),
        elem.attribute("match").map(a => XPathParser.parse(a.text)),
        UserDefinedImportPrecedence
      ))
      .toList // returns a list of tuples (template, name?, match?, precedence)

    new XSLTStylesheet(templates)
  }

  /** Creates a new XSLT template from a [[scala.xml.Elem]] (must be an &lt;xsl:template&gt; element) */
  def parseTemplate(elem: Elem): XSLTTemplate = {
    assert(isElem(elem, "template"))
    new XSLTTemplate(
      parseTemplate(elem.child.filter(n => !isElem(n, "param"))),
      parseParams(elem.child)
    )
  }

  /** Parses a sequence of [[scala.xml.Node]]s as XSLT instructions */
  def parseTemplate(template: Seq[Node]): Seq[XSLTInstruction] = {
    template.map(parseInstruction).toList
  }

  /** Parses a single [[scala.xml.Node]] as an XSLT instruction */
  def parseInstruction(node: Node): XSLTInstruction = node match {
    case text: Text => LiteralTextNode(text.data)
    case elem: Elem => elem.namespace match {
      case Namespace => elem.label match {
        // spec section 11.2
        case "variable" =>
          if (!elem.child.isEmpty) throw new NotImplementedError("Variable definitions are only supported when they use the 'select' attribute")
          // value is empty string '' if there is no select attribute (see XSLT spec section 11.2)
          val select = XPathParser.parse(elem.attribute("select").map(_.text).getOrElse("''"))
          VariableDefinitionInstruction(elem.attribute("name").get.text, select)

        // spec sections 5.4 and 11.6
        case "apply-templates" =>
          val select = elem.attribute("select").map(a => XPathParser.parse(a.text))
          if (!elem.child.forall(isElem(_, "with-param")))
            throw new NotImplementedError("children of 'apply-templates' element must only be 'with-param' ('sort' is not supported)")
          ApplyTemplatesInstruction(select, parseWithParams(elem.child))

        // spec section 6
        case "call-template" =>
          val name = elem.attribute("name").get.text
          if (!elem.child.forall(isElem(_, "with-param")))
            throw new NotImplementedError("children of 'call-templates' element must only be 'with-param'")
          CallTemplatesInstruction(name, parseWithParams(elem.child))

        // spec section 7.1.3
        case "attribute" =>
          // NOTE: attribute value templates are not supported
          val name = elem.attribute("name").get.text
          if (elem.attribute("namespace").isDefined) throw new NotImplementedError("The 'namespace' attribute on xsl:attribute is not supported.")
          SetAttributeInstruction(name, parseTemplate(elem.child)) // NOTE: only text nodes are allowed in the instantiation of this template

        // spec section 7.6.1
        case "value-of" =>
          // <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
          CopyInstruction(XPathParser.parse("string(" + elem.attribute("select").get.text +")"))

        // spec section 11.3
        case "copy-of" =>
          CopyInstruction(XPathParser.parse(elem.attribute("select").get.text))

        // spec section 9.2
        case "choose" =>
          val xsltChildren = elem.child.filter(isElem).map(_.asInstanceOf[Elem])
          val whenBranches = xsltChildren.filter(n => n.label == "when")
            .map(n => (XPathParser.parse(n.attribute("test").get.text), parseTemplate(n.child)))
          val otherwiseBranch = xsltChildren.filter(n => n.label == "otherwise")
            .map(n => parseTemplate(n.child))
            .headOption.getOrElse(Seq())
          ChooseInstruction(whenBranches.toList, otherwiseBranch)

        // spec section 9.1
        case "if" =>
          val test = XPathParser.parse(elem.attribute("test").get.text)
          ChooseInstruction(List((test, parseTemplate(elem.child))), Seq())

        // spec section 7.2 and 3.4 (whitespace stripping)
        case "text" =>
          LiteralTextNode(elem.text)

        case _ => throw new NotImplementedError(f"Unsupported XSLT element: ${elem.label}")
      }
      case null | "" =>
        // element without namespace
        val literalAttributes: Seq[XSLTInstruction] = node.attributes.asAttrMap.map{ case (name, value) => SetAttributeInstruction(name, Seq(LiteralTextNode(value))) }.toSeq
        LiteralElement(node.label, literalAttributes ++ parseTemplate(node.child))
      case _ => throw new NotImplementedError("Namespaces other than the XSLT namespace are not supported.")
    }
    case _ => throw new NotImplementedError(f"Unsupported XML node $node")
  }

  /** Parses &lt;xsl:param&gt; nodes */
  def parseParams(input: Seq[Node]): Map[String, XPathExpr] = {
    // TODO: support content of param element instead of "select" attribute?
    val params = input.filter(isElem(_, "param"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathParser.parse(elem.attribute("select").get.text)))
    Map() ++ params
  }

  /** Parses &lt;xsl:with-param&gt; nodes */
  def parseWithParams(input: Seq[Node]): Map[String, XPathExpr] = {
    // TODO: support content of with-param element instead of "select" attribute?
    // TODO: merge function with parseParams() above
    val params = input.filter(isElem(_, "with-param"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (elem.attribute("name").get.text, XPathParser.parse(elem.attribute("select").get.text)))
    Map() ++ params
  }
}
