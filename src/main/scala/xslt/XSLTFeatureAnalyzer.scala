package xslt

import scala.xml.{Text, Node, Elem}
import scala.collection.mutable.{Set => MutSet}
import xpath._

class UnsupportedFeatureException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

abstract class XSLTFeature
case class Version(v: String) extends XSLTFeature
case object TopLevelVariables extends XSLTFeature
case object TopLevelParams extends XSLTFeature
case object ResultTreeFragments extends XSLTFeature
case class OutputMethod(v: String) extends XSLTFeature // <xsl:output method="...">
case class OtherTopLevelElement(v: String) extends XSLTFeature
case object TemplateModes extends XSLTFeature // <xsl:template mode="...">
case object NamedTemplates extends XSLTFeature // <xsl:template name="...">
case object LiteralText extends XSLTFeature
case object LiteralElements extends XSLTFeature
case object SortInstruction extends XSLTFeature // <xsl:sort>
case object Namespaces extends XSLTFeature
case object AttributeValueTemplates extends XSLTFeature
case object AttributeSets extends XSLTFeature // <xsl:attribute-set>
case object CommentCreation extends XSLTFeature // <xsl:comment>
case object TextCreation extends XSLTFeature // <xsl:text>
case object ElementCreation extends XSLTFeature // <xsl:element>
case object AttributeCreation extends XSLTFeature // <xsl:attribute>
case object ProcessingInstructionCreation extends XSLTFeature // <xsl:processing-instruction>
case object Conditionals extends XSLTFeature // <xsl:if> and <xsl:choose>
case object ForEach extends XSLTFeature // <xsl:for-each>
case class OtherInstruction(v: String) extends XSLTFeature
case object UnionPatterns extends XSLTFeature // union patterns (with '|' operator)
case class DescendantPatterns(trivial: Boolean) extends XSLTFeature // '//' in patterns (trivial if first step)
case object LiteralPositionPredicateInPatterns extends XSLTFeature// predicate of the form [<number>] in patterns
case object AttributeExistencePredicateInPatterns extends XSLTFeature // predicate of the form [@foo] in patterns
case object AttributeLiteralValuePredicateInPatterns extends XSLTFeature // predicate of the form [@foo = 'bar'] in patterns
case object ArbitraryExpressionPredicateInPatterns extends XSLTFeature // any other predicate in patterns

object XSLTFeatureAnalyzer {
  // borrow some functionality from XSLTParser
  val Namespace = XSLTParser.Namespace
  def isElem(node: Node): Boolean = XSLTParser.isElem(node)
  def isElem(node: Node, name: String): Boolean = XSLTParser.isElem(node, name)

  def analyzeFeatures(source: Elem): Set[XSLTFeature] = {
    var f = MutSet[XSLTFeature]()
    val cleaned = XSLTParser.clean(source).asInstanceOf[Elem]
    if (!isElem(cleaned, "stylesheet") && !isElem(cleaned, "transform")) throw new UnsupportedFeatureException(f"Root element must be 'stylesheet' or 'transform' with namespace $Namespace (found ${cleaned.label}, but a literal result element is not supported as root node)")
    val version = cleaned.attribute("version").get.text
    f += Version(version)
    if (version != "1.0" && version != "1.1") throw new UnsupportedFeatureException(f"Only XSLT version 1.x is supported (found version $version")

    cleaned.child.foreach { n =>
      if (isElem(n, "variable")) {
        f += TopLevelVariables
        analyzeVariableOrParamDefinition(n.asInstanceOf[Elem], f)
      } else if (isElem(n, "param")) {
        f += TopLevelParams
        analyzeVariableOrParamDefinition(n.asInstanceOf[Elem], f)
      } else if (isElem(n, "template")) {
        val elem = n.asInstanceOf[Elem]
        if (elem.attribute("mode").isDefined) {
          f += TemplateModes
        }
        if (elem.attribute("name").isDefined) {
          f += NamedTemplates
        }
        if (elem.attribute("match").isDefined) {
          analyzeXPathPattern(XPathParser.parse(elem.attribute("match").get.text), f)
        }
        val (params, rest) = elem.child.partition(isElem(_, "param"))
        analyzeTemplate(rest, f)
        params.foreach { n => analyzeVariableOrParamDefinition(n.asInstanceOf[Elem], f) }
      } else if (isElem(n, "output")) {
        f += OutputMethod(n.asInstanceOf[Elem].attribute("method") match {
          case Some(v) => v.text
          case None => "default"
        })
      } else if (isElem(n, "attribute-set")) {
        f += AttributeSets
        // content contains only <xsl:attribute> nodes, but we can analyze them as if they were a template
        analyzeTemplate(n.child, f)
      } else if (isElem(n)) {
        f += OtherTopLevelElement(n.asInstanceOf[Elem].label)
      }
    }

    f.toSet
  }

  private def analyzeVariableOrParamDefinition(e: Elem, f: MutSet[XSLTFeature]): Unit = {
    if (e.child.nonEmpty) {
      f += ResultTreeFragments
      analyzeTemplate(e.child, f)
    }
  }

  private def analyzeTemplate(nodes: Seq[Node], f: MutSet[XSLTFeature]): Unit = {
    nodes.foreach(analyzeInstruction(_, f))
  }

  private def analyzeInstruction(node: Node, f: MutSet[XSLTFeature]): Unit = {
    node match {
      case text: Text => f += LiteralText
      case elem: Elem => elem.namespace match {
        case Namespace => elem.label match {
          // spec section 11.2
          case "variable" => analyzeVariableOrParamDefinition(elem, f)

          // spec sections 5.4 and 11.6
          case "apply-templates" =>
            if (elem.child.exists(isElem(_, "sort"))) {
              f += SortInstruction
            }
            elem.attribute("select").foreach { a => parseAndAnalyzeXPath(a.text, f) }
            elem.child.filter(n => isElem(n, "with-param")).map(_.asInstanceOf[Elem]).foreach(analyzeVariableOrParamDefinition(_, f))

          // spec section 6
          case "call-template" =>
            elem.child.filter(n => isElem(n, "with-param")).map(_.asInstanceOf[Elem]).foreach(analyzeVariableOrParamDefinition(_, f))

          // spec section 7.1.2
          case "element" =>
            f += ElementCreation
            val name = elem.attribute("name").get.text
            if (name.contains("{") && name.contains("}")) {
              f += AttributeValueTemplates
            }
            if (elem.attribute("namespace").isDefined) {
              f += Namespaces
            }
            analyzeTemplate(elem.child, f)

          // spec section 7.1.3
          case "attribute" =>
            f += AttributeCreation
            val name = elem.attribute("name").get.text
            if (name.contains("{") && name.contains("}")) {
              f += AttributeValueTemplates
            }
            if (elem.attribute("namespace").isDefined) {
              f += Namespaces
            }
            analyzeTemplate(elem.child, f)

            // spec section 7.3
          case "processing-instruction" =>
            f += ProcessingInstructionCreation
            val name = elem.attribute("name").get.text
            if (name.contains("{") && name.contains("}")) {
              f += AttributeValueTemplates
            }
            analyzeTemplate(elem.child, f)

          // spec section 7.4
          case "comment" =>
            f += CommentCreation
            analyzeTemplate(elem.child, f)

          // spec section 9.2
          case "choose" =>
            val xsltChildren = elem.child.filter(isElem).map(_.asInstanceOf[Elem])
            xsltChildren.filter(n => n.label == "when")
              .foreach { n =>
              parseAndAnalyzeXPath(n.attribute("test").get.text, f)
                analyzeTemplate(n.child, f)
              }
            xsltChildren.filter(n => n.label == "otherwise")
              .foreach { n => analyzeTemplate(n.child, f) }

          // spec section 9.1
          case "if" =>
            parseAndAnalyzeXPath(elem.attribute("test").get.text, f)
            analyzeTemplate(elem.child, f)

          // spec section 7.2 and 3.4 (whitespace stripping)
          case "text" =>
            f += TextCreation

          // spec section 7.6.1
          case "value-of" =>
            // <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
            f += OtherInstruction("value-of")
            parseAndAnalyzeXPath(elem.attribute("select").get.text, f)

          // spec section 11.3
          case "copy-of" =>
            f += OtherInstruction("copy-of")
            parseAndAnalyzeXPath(elem.attribute("select").get.text, f)

          // spec section 7.5
          case "copy" =>
            f += OtherInstruction("copy")
            analyzeTemplate(elem.child, f)

          // spec section 8
          case "for-each" =>
            f += ForEach
            elem.attribute("select").foreach { a => parseAndAnalyzeXPath(a.text, f) }

            if (elem.child.exists(isElem(_, "sort"))) {
              f += SortInstruction
            }

            analyzeTemplate(elem.child.filter(!isElem(_, "sort")), f)

          // spec section 13
          case "message" =>
            f += OtherInstruction("message")
            analyzeTemplate(elem.child, f)

          // spec section 7.7
          case "number" =>
            f += OtherInstruction("number")

          case _ => throw new NotImplementedError(f"Unsupported XSLT instruction: ${elem.label}")
        }
        case null | "" =>
          // literal element without namespace
          f += LiteralElements
          analyzeTemplate(elem.child, f)
        case _ =>
          // literal element from other namespace
          f += Namespaces
          f += LiteralElements
          analyzeTemplate(elem.child, f)
      }
      case _ => throw new UnsupportedFeatureException(f"Unsupported XML instruction $node")
    }
  }

  private def parseAndAnalyzeXPath(str: String, f: MutSet[XSLTFeature]): Unit = {
    analyzeXPathExpression(XPathParser.parse(str), f)
  }

  private def analyzeXPathExpression(expr: XPathExpr, f: MutSet[XSLTFeature]): Unit = {
    // TODO: implement analysis
  }

  private def analyzeXPathPattern(expr: XPathExpr, f: MutSet[XSLTFeature]): Unit = {
    def analyzeSinglePattern(path: LocationPath) = {
      path.steps.take(1).foreach(analyzeSingleStep(_, true))
      path.steps.drop(1).foreach(analyzeSingleStep(_, false))
    }

    def analyzeSingleStep(step: XPathStep, isFirst: Boolean) = {
      step match {
        case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) => f += DescendantPatterns(isFirst)
        case _ => ()
      }
      step.predicates.foreach(analyzePredicate)
    }

    def analyzePredicate(pred: XPathExpr) = {
      def isAttributePath(expr: XPathExpr) = expr match {
        case LocationPath(XPathStep(AttributeAxis, NameTest(_, _), Nil) :: Nil, false) => true
        case _ => false
      }

      def isLiteralAttributeValue(expr: XPathExpr) = expr match {
        case LiteralExpr(_) => true
        case NumberExpr(_) => true
        case _ => false
      }

      pred match {
        case NumberExpr(_) => f += LiteralPositionPredicateInPatterns // predicate of the form [0]
        case _ if isAttributePath(pred) => f += AttributeExistencePredicateInPatterns // predicate of the form [@foobar]
        case RelationalExpr(lhs, rhs, EqualsOperator) => // predicate of the form [@foo = 'bar']
          if ((isAttributePath(lhs) && isLiteralAttributeValue(rhs)) ||
            (isAttributePath(rhs) && isLiteralAttributeValue(lhs))) {
            f += AttributeLiteralValuePredicateInPatterns
          }
        case _ =>
          f += ArbitraryExpressionPredicateInPatterns
          analyzeXPathExpression(pred, f)
      }
    }

    if (expr.isInstanceOf[UnionExpr]) {
        f += UnionPatterns
    }

    XPathExpr.splitUnionPattern(expr).foreach(analyzeSinglePattern)
  }
}
