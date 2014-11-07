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

// TODO: use this instead of Option with None?
/*case object AnyNode extends PatternStepDescriptor*/

abstract class XPathPattern

case object Root extends XPathPattern {
  override def toString = "/"
}

case object AnyNode extends XPathPattern {
  override def toString = "<ANY>"
}

case class Step(descriptor: PatternStepDescriptor, previous: XPathPattern) extends XPathPattern {
  def withPrev(pattern: XPathPattern) = Step(this.descriptor, pattern)
  override def toString = previous match {
    case AnyNode => descriptor.toString
    case Root => "/" + descriptor.toString
    case p => p.toString + "/" + descriptor.toString
  }
}

object XPathPattern {
  def fromString(str: String) = fromLocationPath(XPathParser.parse(str).asInstanceOf[LocationPath])

  def fromLocationPath(path: LocationPath): XPathPattern = fromLocationPath(path.steps.reverse, path.isAbsolute)

  private def fromLocationPath(reverseSteps: List[XPathStep], isAbsolute: Boolean): XPathPattern = {
    reverseSteps match {
      case Nil => if (isAbsolute) Root else AnyNode
      case next :: rest =>
        val desc = next match {
          case XPathStep(ChildAxis, CommentNodeTest, Nil) => AnyCommentNode
          case XPathStep(ChildAxis, TextNodeTest, Nil) => AnyTextNode
          case XPathStep(ChildAxis, NameTest("*"), Nil) => AnyElement
          case XPathStep(ChildAxis, NameTest(name), Nil) => NamedElement(name)
          case XPathStep(AttributeAxis, NameTest("*"), Nil) => AnyAttribute
          case XPathStep(AttributeAxis, NameTest(name), Nil) => NamedAttribute(name)
          case _ => throw new UnsupportedOperationException(f"Step $next can not be translated to this domain.")
        }
        Step(desc, fromLocationPath(rest, isAbsolute))
    }
  }
}