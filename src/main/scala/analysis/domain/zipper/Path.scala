package analysis.domain.zipper

import analysis.domain.Lattice

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

abstract class Path

case object RootPath extends Path {
  override def toString = "/"
}

case class ChildStep(descriptor: PatternStepDescriptor, parent: Path) extends Path {
  override def toString = parent match {
    case RootPath => "/" + descriptor.toString
    case p => p.toString + "/" + descriptor.toString
  }
}

case class DescendantStep(descriptor: PatternStepDescriptor, ancestor: Path) extends Path {
  override def toString = ancestor match {
    case RootPath => descriptor.toString
    case p => p.toString + "//" + descriptor.toString
  }
}

/** Helper object to parse patterns. */
object Path {
  import xpath._

  def fromString(str: String) = fromLocationPath(XPathParser.parse(str).asInstanceOf[LocationPath])

  def fromLocationPath(path: LocationPath): Path = fromLocationPath(path.steps.reverse, path.isAbsolute)

  private def fromLocationPath(reverseSteps: List[XPathStep], isAbsolute: Boolean): Path = {
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
      case Nil if isAbsolute => RootPath
      case Nil => throw new UnsupportedOperationException("Empty relative paths cannot exist.")
      case next :: Nil if !isAbsolute =>
        DescendantStep(parseStep(next), RootPath) // last step of a relative path -> descendant of root
      case next :: XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: rest =>
        DescendantStep(parseStep(next), fromLocationPath(rest, isAbsolute))
      case next :: rest => // default case
        ChildStep(parseStep(next), fromLocationPath(rest, isAbsolute))
    }
  }

  implicit object PathSetLattice extends Lattice[Set[Path]] {
    def top = Set(RootPath,
      DescendantStep(AnyElement, RootPath),
      DescendantStep(AnyAttribute, RootPath),
      DescendantStep(AnyCommentNode, RootPath),
      DescendantStep(AnyTextNode, RootPath))

    def bottom = Set()

    def join(n1: Set[Path], n2: Set[Path]): Set[Path] = normalize(n1.union(n2))

    protected def normalize(set: Iterable[Path]): Set[Path] = set.toList.foldRight(List[Path]()) {
      case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(next, e)))
        acc
      else
        next :: acc.filter(e => !lessThanOrEqualSingle(e, next))
    }.toSet

    def meet(n1: Set[Path], n2: Set[Path]): Set[Path] = {
      val result = n1.cross(n2).flatMap {
        case (p1, p2) => meetSingle(p1, p2)
      }
      normalize(result.toSet)
    }

    protected def meetSingle(left: Path, right: Path): Set[Path] = (left, right) match {
      case (RootPath, RootPath) => Set(RootPath)
      case (ChildStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        meetSingle(prev1, prev2).flatMap { r =>
          meetDescriptor(desc1, desc2) match {
            case None => return Set() // BOTTOM
            case Some(desc) => Set(ChildStep(desc, r))
          }
        }
      case (DescendantStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => DescendantStep(desc, r)).asInstanceOf[Set[Path]] |
            meetSingle(DescendantStep(AnyElement, prev1), prev2).map(r => DescendantStep(desc, r)) |
            meetSingle(prev1, DescendantStep(AnyElement, prev2)).map(r => DescendantStep(desc, r))
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[Path]] |
            meetSingle(DescendantStep(AnyElement, prev1), prev2).map(r => ChildStep(desc, r))
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[Path]] |
            meetSingle(prev1, DescendantStep(AnyElement, prev2)).map(r => ChildStep(desc, r))
        }
      case _ => Set() // mix of Root and Step -> BOTTOM
    }

    protected def meetDescriptor(desc1: PatternStepDescriptor, desc2: PatternStepDescriptor): Option[PatternStepDescriptor] = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => Some(desc1)
        case (NamedElement(name), AnyElement) => Some(NamedElement(name))
        case (AnyElement, NamedElement(name)) => Some(NamedElement(name))
        case (NamedAttribute(name), AnyAttribute) => Some(NamedAttribute(name))
        case (AnyAttribute, NamedAttribute(name)) => Some(NamedAttribute(name))
        case _ => None // represents BOTTOM here
      }
    }

    def lessThanOrEqual(n1: Set[Path], n2: Set[Path]): Boolean =
      n1.forall(pat1 => n2.exists(pat2 => lessThanOrEqualSingle(pat1, pat2)))

    protected def lessThanOrEqualSingle(left: Path, right: Path): Boolean = (left, right) match {
      case (RootPath, RootPath) => true
      case (ChildStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && lessThanOrEqualSingle(prev1, prev2)
      case (DescendantStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElement, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElement, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) => false
      case _ => false // mix of Root and Step
    }

    protected def lessThanOrEqualDescriptor(desc1: PatternStepDescriptor, desc2: PatternStepDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (NamedElement(_), AnyElement) => true // less than
        case (NamedAttribute(_), AnyAttribute) => true // less than
        case _ => false
      }
    }

    def getParent(path: Set[Path]) = normalize(path.collect {
      case ChildStep(desc, prev) => List(prev)
      case DescendantStep(desc, prev) => prev match {
        case RootPath => desc match {
          // NOTE: only elements can have Root as their parent
          case AnyElement | NamedElement(_) => List(RootPath, DescendantStep(AnyElement, RootPath))
          case _ => List(DescendantStep(AnyElement, RootPath))
        }
        case p => List(p, DescendantStep(AnyElement, p))
      }
    }.flatten)

    def getChildren(path: Set[Path]): ZList[Set[Path]] = ZList.join(path.map {
      case e@ChildStep(AnyElement, _) => ZUnknownLength(Set[Path](ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e)))
      case e@ChildStep(NamedElement(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e)))
      case e@DescendantStep(AnyElement, _) => ZUnknownLength(Set[Path](ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e)))
      case e@DescendantStep(NamedElement(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e)))
      case e@RootPath => ZCons(Set[Path](ChildStep(AnyElement, e)), ZNil())
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have children
    })

    def getAttributes(path: Set[Path]): ZList[Set[Path]] = ZList.join(path.map {
      case e@ChildStep(AnyElement, _) => ZUnknownLength(Set[Path](ChildStep(AnyAttribute, e)))
      case e@ChildStep(NamedElement(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttribute, e)))
      case e@DescendantStep(AnyElement, _) => ZUnknownLength(Set[Path](ChildStep(AnyAttribute, e)))
      case e@DescendantStep(NamedElement(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttribute, e)))
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have attributes
    })

    def isElement(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(NamedElement(_), _) => true
        case ChildStep(AnyElement, _) => true
        case DescendantStep(NamedElement(_), _) => true
        case DescendantStep(AnyElement, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isTextNode(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(AnyTextNode, _) => true
        case DescendantStep(AnyTextNode, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isComment(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(AnyCommentNode, _) => true
        case DescendantStep(AnyCommentNode, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isAttribute(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(NamedAttribute(_), _) => true
        case ChildStep(AnyAttribute, _) => true
        case DescendantStep(NamedAttribute(_), _) => true
        case DescendantStep(AnyAttribute, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }
  }
}