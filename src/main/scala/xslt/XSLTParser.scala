package xslt

import xpath._

import scala.xml._

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

  /** Preprocesses a [[scala.xml.Node]] by removing whitespace-only text-nodes (except inside &lt;xsl:text&gt;) and comment nodes. */
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

  def parseStylesheet(source: Elem, disableBuiltinTemplates: Boolean = false): XSLTStylesheet = {
    val cleaned = clean(source).asInstanceOf[Elem]

    if (!isElem(cleaned, "stylesheet") && !isElem(cleaned, "transform"))
      throw new NotImplementedError(f"Root element must be 'stylesheet' or 'transform' with namespace $Namespace (a literal result element is not supported as root node)")
    if (cleaned.attribute("version").get.text != "1.0")
      throw new NotImplementedError("Stylesheet versions other than 1.0 are not supported")

    cleaned.child.foreach {
      n =>
        require(n.namespace == Namespace, f"Top-level elements must be from XSL namespace, got $n")
        if (n.label != "template" && n.label != "variable" && n.label != "param") {
          println(f"Warning: Ignoring unsupported top-level element ${n}")
        }
    }

    // collect global variables and treat top-level parameters as if they were variables (using their mandatory default values)
    // NOTE: the spec requires us to evaluate top-level variables in an order such that variables can depend on each other
    // (as long as there are no circular dependencies, which would result in an error), see spec section 11.4
    // this has not been implemented!
    val globalVariables = parseParams(cleaned.child.filter(e => isElem(e, "variable") || isElem(e, "param")))

    val templates = cleaned.child
      .filter(isElem(_, "template"))
      .map(n => n.asInstanceOf[Elem])
      .map(elem => (parseTemplate(elem),
        elem.attribute("name").map(_.text),
        elem.attribute("match").map(a => XPathParser.parse(a.text)),
        elem.attribute("mode").map(_.text)
      ))
      .toList // returns a list of tuples (template, name?, match?, precedence)

    new XSLTStylesheet(templates, globalVariables, disableBuiltinTemplates)
  }

  /** Creates a new XSLT template from a [[scala.xml.Elem]] (must be an &lt;xsl:template&gt; element) */
  def parseTemplate(elem: Elem): XSLTTemplate = {
    assert(isElem(elem, "template"))
    new XSLTTemplate(
      parseInstructions(elem.child.filter(n => !isElem(n, "param"))),
      parseParams(elem.child.filter(isElem(_, "param")))
    )
  }

  /** Parses a sequence of [[scala.xml.Node]]s as XSLT instructions */
  def parseInstructions(template: Seq[Node]): Seq[XSLTInstruction] = {
    template.map(parseInstruction).toList
  }

  /** Parses a single [[scala.xml.Node]] as an XSLT instruction */
  def parseInstruction(node: Node): XSLTInstruction = node match {
    case text: Text => CreateTextInstruction(text.data)
    case entity: EntityRef => CreateTextInstruction(entity.text)
    case elem: Elem => elem.namespace match {
      case Namespace => elem.label match {
        // spec section 11.2
        case "variable" =>
          val value = (elem.attribute("select"), elem.child.isEmpty) match {
            case (Some(select), _) => Left(XPathParser.parse(select.text))
            case (None, false) => Right(parseInstructions(elem.child))
            case (None, true) => Left(StringLiteralExpr("")) // value is empty string '' if there is no select attribute (see XSLT spec section 11.2)
          }
          VariableDefinitionInstruction(elem.attribute("name").get.text, value)

        // spec sections 5.4 and 11.6
        case "apply-templates" =>
          val select = elem.attribute("select").map(a => XPathParser.parse(a.text))
          val mode = elem.attribute("mode").map(_.text)
          if (!elem.child.forall(isElem(_, "with-param")))
            throw new NotImplementedError("children of 'apply-templates' element must only be 'with-param' ('sort' is not supported)")
          ApplyTemplatesInstruction(select, mode, Map() ++ parseParams(elem.child.filter(isElem(_, "with-param"))))

        // spec section 6
        case "call-template" =>
          val name = elem.attribute("name").get.text
          if (!elem.child.forall(isElem(_, "with-param")))
            throw new NotImplementedError("children of 'call-templates' element must only be 'with-param'")
          CallTemplateInstruction(name, Map() ++ parseParams(elem.child.filter(isElem(_, "with-param"))))

        // spec section 7.1.2
        case "element" =>
          val name = XPathParser.parseAttributeValueTemplate(elem.attribute("name").get.text)
          if (elem.attribute("namespace").isDefined) throw new NotImplementedError("The 'namespace' attribute on xsl:element is not supported.")
          CreateElementInstruction(name, parseInstructions(elem.child))

        // spec section 7.1.3
        case "attribute" =>
          val name = XPathParser.parseAttributeValueTemplate(elem.attribute("name").get.text)
          if (elem.attribute("namespace").isDefined) throw new NotImplementedError("The 'namespace' attribute on xsl:attribute is not supported.")
          SetAttributeInstruction(name, parseInstructions(elem.child)) // NOTE: only text nodes are allowed in the instantiation of this template

        // spec section 7.4
        case "comment" =>
          CreateCommentInstruction(parseInstructions(elem.child))

        // spec section 7.5
        case "copy" => CopyInstruction(parseInstructions(elem.child))

        // spec section 7.6.1
        case "value-of" =>
          // <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
          CopyOfInstruction(XPathParser.parse("string(" + elem.attribute("select").get.text +")"))

        // spec section 11.3
        case "copy-of" =>
          CopyOfInstruction(XPathParser.parse(elem.attribute("select").get.text))

        // spec section 9.2
        case "choose" =>
          val xsltChildren = elem.child.filter(isElem).map(_.asInstanceOf[Elem])
          val whenBranches = xsltChildren.filter(n => n.label == "when")
            .map(n => (XPathParser.parse(n.attribute("test").get.text), parseInstructions(n.child)))
          val otherwiseBranch = xsltChildren.filter(n => n.label == "otherwise")
            .map(n => parseInstructions(n.child))
            .headOption.getOrElse(Seq())
          ChooseInstruction(whenBranches.toList, otherwiseBranch)

        // spec section 9.1
        case "if" =>
          val test = XPathParser.parse(elem.attribute("test").get.text)
          ChooseInstruction(List((test, parseInstructions(elem.child))), Seq())

        // spec section 7.2 and 3.4 (whitespace stripping)
        case "text" =>
          CreateTextInstruction(elem.text)

        // spec section 8
        case "for-each" =>
          if (elem.child.exists(isElem(_, "sort")))
            throw new NotImplementedError("'sort' is not supporte (found inside 'for-each')")
          ForEachInstruction(XPathParser.parse(elem.attribute("select").get.text), parseInstructions(elem.child))

        case "number" =>
          NumberInstruction() // just a dummy

        case _ => throw new NotImplementedError(f"Unsupported XSLT instruction: ${elem.label}")
      }
      case null | "" =>
        // element without namespace
        val literalAttributes: Seq[XSLTInstruction] = node.attributes.asAttrMap.map { case (name, value) =>
          val valueExpr = attributeValueTemplateToExpression(XPathParser.parseAttributeValueTemplate(value))
          SetAttributeInstruction(List(Left(name)), Seq(CopyOfInstruction(valueExpr)))
        }.toSeq
        CreateElementInstruction(List(Left(node.label)), literalAttributes ++ parseInstructions(node.child))
      case _ => throw new NotImplementedError(f"Namespaces other than the XSLT namespace are not supported ($node).")
    }
    case _ => throw new NotImplementedError(f"Unsupported XML node $node")
  }

  /** Parses &lt;xsl:param&gt; and &lt;xsl:with-param&gt; nodes */
  def parseParams(input: Seq[Node]): List[(String, Either[XPathExpr, Seq[XSLTInstruction]])] = {
    input.map(n => n.asInstanceOf[Elem])
      .map { elem =>
        elem.attribute("name").get.text -> ((elem.attribute("select"), elem.child.isEmpty) match {
        case (Some(select), _) => Left(XPathParser.parse(select.text))
        case (None, false) => Right(parseInstructions(elem.child))
        case (None, true) => Left(StringLiteralExpr(""))
    })}.toList
  }

  /** Lowers a parsed attribute value template to a single XPath expression that evaluates
    * to the same result, using the concat function if necessary.
    */
  def attributeValueTemplateToExpression(avt: List[Either[String, XPathExpr]]): XPathExpr = {
    val exprList = avt.map {
      case Left(str) => StringLiteralExpr(str)
      case Right(expr) => FunctionCallExpr(None, "string", List(expr))
    }
    // NOTE: we need to make sure that the result (and the arguments to concat) are strings
    exprList match {
      case single :: Nil => single
      case _ => FunctionCallExpr(None, "concat", exprList)
    }
  }
}
