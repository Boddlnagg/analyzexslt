package analysis.domain.path

import xpath._

abstract class PatternStepDescriptor
case object AnyElement extends PatternStepDescriptor {
  override def toString = "*"
}
case class NamedElement(name: String) extends PatternStepDescriptor {
  override def toString = name
}
case object AnyAttribute extends PatternStepDescriptor {
  override def toString = "@*"
}
case class NamedAttribute(name: String) extends PatternStepDescriptor {
  override def toString = "@" + name
}
case object AnyTextNode extends PatternStepDescriptor {
  override def toString = "text()"
}
case object AnyCommentNode extends PatternStepDescriptor {
  override def toString = "comment()"
}

abstract class XPathPattern

case object Root extends XPathPattern {
  override def toString = "/"
}

case class ChildStep(descriptor: PatternStepDescriptor, parent: XPathPattern) extends XPathPattern {
  override def toString = parent match {
    case Root => "/" + descriptor.toString
    case p => p.toString + "/" + descriptor.toString
  }
}

case class DescendantStep(descriptor: PatternStepDescriptor, ancestor: XPathPattern) extends XPathPattern {
  override def toString = ancestor match {
    case Root => descriptor.toString
    case p => p.toString + "//" + descriptor.toString
  }
}

object XPathPattern {
  def fromString(str: String) = fromLocationPath(XPathParser.parse(str).asInstanceOf[LocationPath])

  def fromLocationPath(path: LocationPath): XPathPattern = fromLocationPath(path.steps.reverse, path.isAbsolute)

  private def fromLocationPath(reverseSteps: List[XPathStep], isAbsolute: Boolean): XPathPattern = {
    def parseStep(step: XPathStep) = step match {
      case XPathStep(ChildAxis, CommentNodeTest, Nil) => AnyCommentNode
      case XPathStep(ChildAxis, TextNodeTest, Nil) => AnyTextNode
      case XPathStep(ChildAxis, NameTest("*"), Nil) => AnyElement
      case XPathStep(ChildAxis, NameTest(name), Nil) => NamedElement(name)
      case XPathStep(AttributeAxis, NameTest("*"), Nil) => AnyAttribute
      case XPathStep(AttributeAxis, NameTest(name), Nil) => NamedAttribute(name)
      case _ => throw new UnsupportedOperationException(f"Step $step can not be translated to this domain.")
    }

    reverseSteps match {
      case Nil if isAbsolute => Root
      case Nil => throw new UnsupportedOperationException("Empty relative paths cannot exist.")
      case next :: Nil if !isAbsolute =>
        DescendantStep(parseStep(next), Root) // last step of a relative path -> descendant of root
      case next :: XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: rest =>
        DescendantStep(parseStep(next), fromLocationPath(rest, isAbsolute))
      case next :: rest => // default case
        ChildStep(parseStep(next), fromLocationPath(rest, isAbsolute))
    }
  }
}