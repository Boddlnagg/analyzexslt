package analysis.domain.zipper

import analysis.domain.Lattice

abstract class PathStepDescriptor

case object AnyElementStep extends PathStepDescriptor {
  override def toString = "*"
}
case class NamedElementStep(name: String) extends PathStepDescriptor {
  override def toString = name
}
case object AnyAttributeStep extends PathStepDescriptor {
  override def toString = "@*"
}
case class NamedAttributeStep(name: String) extends PathStepDescriptor {
  override def toString = "@" + name
}
case object AnyTextNodeStep extends PathStepDescriptor {
  override def toString = "text()"
}
case object AnyCommentNodeStep extends PathStepDescriptor {
  override def toString = "comment()"
}

abstract class Path

case object RootPath extends Path {
  override def toString = "/"
}

case class ChildStep(descriptor: PathStepDescriptor, parent: Path) extends Path {
  override def toString = parent match {
    case RootPath => "/" + descriptor.toString
    case p => p.toString + "/" + descriptor.toString
  }
}

case class DescendantStep(descriptor: PathStepDescriptor, ancestor: Path) extends Path {
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
      case XPathStep(ChildAxis, CommentNodeTest, Nil) => AnyCommentNodeStep
      case XPathStep(ChildAxis, TextNodeTest, Nil) => AnyTextNodeStep
      case XPathStep(ChildAxis, NameTest("*"), Nil) => AnyElementStep
      case XPathStep(ChildAxis, NameTest(name), Nil) => NamedElementStep(name)
      case XPathStep(AttributeAxis, NameTest("*"), Nil) => AnyAttributeStep
      case XPathStep(AttributeAxis, NameTest(name), Nil) => NamedAttributeStep(name)
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
      DescendantStep(AnyElementStep, RootPath),
      DescendantStep(AnyAttributeStep, RootPath),
      DescendantStep(AnyCommentNodeStep, RootPath),
      DescendantStep(AnyTextNodeStep, RootPath))

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
            meetSingle(DescendantStep(AnyElementStep, prev1), prev2).map(r => DescendantStep(desc, r)) |
            meetSingle(prev1, DescendantStep(AnyElementStep, prev2)).map(r => DescendantStep(desc, r))
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[Path]] |
            meetSingle(DescendantStep(AnyElementStep, prev1), prev2).map(r => ChildStep(desc, r))
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[Path]] |
            meetSingle(prev1, DescendantStep(AnyElementStep, prev2)).map(r => ChildStep(desc, r))
        }
      case _ => Set() // mix of Root and Step -> BOTTOM
    }

    protected def meetDescriptor(desc1: PathStepDescriptor, desc2: PathStepDescriptor): Option[PathStepDescriptor] = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => Some(desc1)
        case (d@NamedElementStep(_), AnyElementStep) => Some(d)
        case (AnyElementStep, d@NamedElementStep(_)) => Some(d)
        case (d@NamedAttributeStep(_), AnyAttributeStep) => Some(d)
        case (AnyAttributeStep, d@NamedAttributeStep(_)) => Some(d)
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
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElementStep, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElementStep, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) => false
      case _ => false // mix of Root and Step
    }

    protected def lessThanOrEqualDescriptor(desc1: PathStepDescriptor, desc2: PathStepDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (NamedElementStep(_), AnyElementStep) => true // less than
        case (NamedAttributeStep(_), AnyAttributeStep) => true // less than
        case _ => false
      }
    }

    def getParent(path: Set[Path]) = normalize(path.collect {
      case ChildStep(desc, prev) => List(prev)
      case DescendantStep(desc, prev) => prev match {
        case RootPath => desc match {
          // NOTE: only elements can have Root as their parent
          case AnyElementStep | NamedElementStep(_) => List(RootPath, DescendantStep(AnyElementStep, RootPath))
          case _ => List(DescendantStep(AnyElementStep, RootPath))
        }
        case p => List(p, DescendantStep(AnyElementStep, p))
      }
    }.flatten)

    def getChildren(path: Set[Path]): ZList[Set[Path]] = ZList.join(path.map {
      case e@ChildStep(AnyElementStep, _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@ChildStep(NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@DescendantStep(AnyElementStep, _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@DescendantStep(NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@RootPath => ZCons(Set[Path](ChildStep(AnyElementStep, e)), ZNil())
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have children
    })

    def getAttributes(path: Set[Path]): ZList[Set[Path]] = ZList.join(path.map {
      case e@ChildStep(AnyElementStep, _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case e@ChildStep(NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case e@DescendantStep(AnyElementStep, _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case e@DescendantStep(NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have attributes
    })

    def isElement(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(NamedElementStep(_), _) => true
        case ChildStep(AnyElementStep, _) => true
        case DescendantStep(NamedElementStep(_), _) => true
        case DescendantStep(AnyElementStep, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isTextNode(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(AnyTextNodeStep, _) => true
        case DescendantStep(AnyTextNodeStep, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isComment(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(AnyCommentNodeStep, _) => true
        case DescendantStep(AnyCommentNodeStep, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def isAttribute(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(NamedAttributeStep(_), _) => true
        case ChildStep(AnyAttributeStep, _) => true
        case DescendantStep(NamedAttributeStep(_), _) => true
        case DescendantStep(AnyAttributeStep, _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def hasName(node: Set[Path], name: String): (Set[Path], Set[Path]) = (normalize(node.collect {
      case e@ChildStep(NamedElementStep(n), _) if name == n => e
      case ChildStep(AnyElementStep, p) => ChildStep(NamedElementStep(name), p)
      case a@ChildStep(NamedAttributeStep(n), _) if name == n => a
      case ChildStep(AnyAttributeStep, p) => ChildStep(NamedAttributeStep(name), p)
      case e@DescendantStep(NamedElementStep(n), _) if name == n => e
      case DescendantStep(AnyElementStep, p) => DescendantStep(NamedElementStep(name), p)
      case a@DescendantStep(NamedAttributeStep(n), _) if name == n => a
      case DescendantStep(AnyAttributeStep, p) => DescendantStep(NamedAttributeStep(name), p)
    }), node) // TODO: negative result
  }
}