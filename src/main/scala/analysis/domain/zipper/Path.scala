package analysis.domain.zipper

import analysis.domain.Lattice

/** Base class for descriptors (labels) of path steps. A descriptor describes a single node. */
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

/** Base class for paths. */
abstract class Path

/** The root path describes the root node. */
case object RootPath extends Path {
  override def toString = "/"
}

/** The child step describes a node that is a child of another node.
  * It contains a descriptor of the node itself and the parent path.
  */
case class ChildStep(descriptor: PathStepDescriptor, parent: Path) extends Path {
  override def toString = parent match {
    case RootPath => "/" + descriptor.toString
    case p => p.toString + "/" + descriptor.toString
  }
}

/** The descendant step describes a node that is a descendant of another node.
  * It contains a descriptor of the node itself and the ancestor path, describing any ancestor.
  */
case class DescendantStep(descriptor: PathStepDescriptor, ancestor: Path) extends Path {
  override def toString = ancestor match {
    case RootPath => descriptor.toString
    case p => p.toString + "//" + descriptor.toString
  }
}

/** Helper object that contains
  * - methods to parse patterns from XPath expressions
  * - an implementation of the Lattice type class for sets of paths
  */
object Path {
  import xpath._

  /** Creates a [[Path]] object from a given string, by first parsing the string using the [[XPathParser]]. */
  def fromString(str: String) = fromLocationPath(XPathParser.parse(str).asInstanceOf[LocationPath])

  /** Creates a [[Path]] object given a [[LocationPath]]. */
  def fromLocationPath(path: LocationPath): Path = fromLocationPath(path.steps.reverse, path.isAbsolute)

  private def fromLocationPath(reverseSteps: List[XPathStep], isAbsolute: Boolean): Path = {
    def parseStep(step: XPathStep) = step match {
      case XPathStep(ChildAxis, CommentNodeTest, Nil) => AnyCommentNodeStep
      case XPathStep(ChildAxis, TextNodeTest, Nil) => AnyTextNodeStep
      case XPathStep(ChildAxis, NameTest(None, "*"), Nil) => AnyElementStep
      case XPathStep(ChildAxis, NameTest(None, name), Nil) => NamedElementStep(name)
      case XPathStep(AttributeAxis, NameTest(None, "*"), Nil) => AnyAttributeStep
      case XPathStep(AttributeAxis, NameTest(None, name), Nil) => NamedAttributeStep(name)
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

  /** This defines operations on the lattice of sets of abstract paths. */
  implicit object PathSetLattice extends Lattice[Set[Path]] {

    /** This describes every possible path, because every node is either
      * - the root node itself or
      * - an element, attribute, comment or text node that is a descendant of the root node
      */
    def top = Set(RootPath,
      DescendantStep(AnyElementStep, RootPath),
      DescendantStep(AnyAttributeStep, RootPath),
      DescendantStep(AnyCommentNodeStep, RootPath),
      DescendantStep(AnyTextNodeStep, RootPath))

    def bottom = Set()

    /** Defines the JOIN operation for the lattice of path-sets. */
    def join(n1: Set[Path], n2: Set[Path]): Set[Path] = normalize(n1.union(n2))

    /** Normalize the given set of paths in such a way that the resulting set contains
      * no path that also describes (i.e. is greater than or equal to) another path in the set.
      */
    protected def normalize(set: Iterable[Path]): Set[Path] = set.toList.foldRight(List[Path]()) {
      case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(next, e)))
        acc
      else
        next :: acc.filter(e => !lessThanOrEqualSingle(e, next))
    }.toSet

    /** Defines the MEET operation (&&) for the lattice of path-sets. */
    def meet(n1: Set[Path], n2: Set[Path]): Set[Path] = {
      val result = n1.cross(n2).flatMap {
        case (p1, p2) => meetSingle(p1, p2)
      }
      normalize(result.toSet)
    }

    /** Defines the MEET operation (&&) for single paths. The result is a set of paths with either
      * - exactly one element, which is a path that was described by both input paths
      *   (i.e. less than or equal to both operands)
      * - no element (= BOTTOM), when no such common path exists
      *
      * Examples: /@foobar && /@* = Set(/@foobar), /a && /a/b = Set()
      */
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

    /** Defines the MEET operation (&&) for descriptors. The result is
      * - 'Some(desc)' with 'desc' being a descriptor that describes nodes that are described by both operands
      * - 'None' if no such common descriptor exists (= BOTTOM)
      *
      * Examples: @foobar && @* = Some(@foobar), @foobar && * = None
      */
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

    /** Defines the <= operation for the lattice of path-sets */
    def lessThanOrEqual(n1: Set[Path], n2: Set[Path]): Boolean =
      n1.forall(pat1 => n2.exists(pat2 => lessThanOrEqualSingle(pat1, pat2)))

    /** Check if a single path is less than or equal to another path,
      * i.e. if the left path is also described by the right path.
      *
      * Examples: /@foobar <= /@*, elem/@foobar <= elem//@foobar
      */
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

    /** Check if a descriptor is less than or equal to another descriptor,
      * i.e. if the right descriptor also describes all nodes that the left one describes.
      * Example: @foobar <= @* (for attribute descriptors)
      */
    protected def lessThanOrEqualDescriptor(desc1: PathStepDescriptor, desc2: PathStepDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (NamedElementStep(_), AnyElementStep) => true // less than
        case (NamedAttributeStep(_), AnyAttributeStep) => true // less than
        case _ => false
      }
    }

    /** Gets the path of the parent node of a given path. */
    def getParent(path: Set[Path]): Set[Path] = normalize(path.collect {
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

    /** Gets a list of the paths of all child nodes of a given path. */
    def getChildren(path: Set[Path]): ZList[Set[Path]] = ZList.joinAll(path.map {
      case e@ChildStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@DescendantStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
      case e@RootPath => ZCons(Set[Path](ChildStep(AnyElementStep, e)), ZNil()) //ZUnknownLength(Set[Path](ChildStep(AnyElementStep, e), ChildStep(AnyTextNodeStep, e), ChildStep(AnyCommentNodeStep, e)))
        // NOTE: for real documents we could return ZCons(Set[Path](ChildStep(AnyElementStep, e)), ZNil())
        // for RootPaths, but this won't work for Result Tree Fragments
        // TODO: improve this situation
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have children
    })

    /** Gets a list of the paths of all descendant nodes of a given path. */
    def getDescendants(path: Set[Path]): ZList[Set[Path]] = ZList.joinAll(path.map {
      case e@ChildStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](DescendantStep(AnyElementStep, e), DescendantStep(AnyTextNodeStep, e), DescendantStep(AnyCommentNodeStep, e)))
      case e@DescendantStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](DescendantStep(AnyElementStep, e), DescendantStep(AnyTextNodeStep, e), DescendantStep(AnyCommentNodeStep, e)))
      case e@RootPath => ZCons(Set[Path](DescendantStep(AnyElementStep, e)), ZNil())
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have children/descendants
    })

    /** Gets a list of the paths of all attributes of a given node (described by its path). */
    def getAttributes(path: Set[Path]): ZList[Set[Path]] = ZList.joinAll(path.map {
      case e@ChildStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case e@DescendantStep(AnyElementStep | NamedElementStep(_), _) => ZUnknownLength(Set[Path](ChildStep(AnyAttributeStep, e)))
      case _ => ZNil[Set[Path]]() // other node types (text, attribute) return empty lists because they don't have attributes
    })

    // The following predicate functions could probably be removed, because the combination of these predicates
    // for NodeDescriptor together with the normalization function make them unnecessary.

    def isElement(node: Set[Path]): (Set[Path], Set[Path]) = {
      val (yes, no) = node.partition {
        case ChildStep(AnyElementStep | NamedElementStep(_), _) => true
        case DescendantStep(AnyElementStep | NamedElementStep(_), _) => true
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
        case ChildStep(AnyAttributeStep | NamedAttributeStep(_), _) => true
        case DescendantStep(AnyAttributeStep | NamedAttributeStep(_), _) => true
        case _ => false
      }
      (normalize(yes), normalize(no))
    }

    def hasName(node: Set[Path], name: String): (Set[Path], Set[Path]) = {
      val yes = node.collect {
        case e@ChildStep(NamedElementStep(n), _) if name == n => e
        case ChildStep(AnyElementStep, p) => ChildStep(NamedElementStep(name), p)
        case a@ChildStep(NamedAttributeStep(n), _) if name == n => a
        case ChildStep(AnyAttributeStep, p) => ChildStep(NamedAttributeStep(name), p)
        case e@DescendantStep(NamedElementStep(n), _) if name == n => e
        case DescendantStep(AnyElementStep, p) => DescendantStep(NamedElementStep(name), p)
        case a@DescendantStep(NamedAttributeStep(n), _) if name == n => a
        case DescendantStep(AnyAttributeStep, p) => DescendantStep(NamedAttributeStep(name), p)
      }

      val no = node.collect {
        case e@ChildStep(NamedElementStep(n), _) if name != n => e
        case e@ChildStep(AnyElementStep, p) => e // can't express that name is *not* the specified one
        case a@ChildStep(NamedAttributeStep(n), _) if name != n => a
        case a@ChildStep(AnyAttributeStep, p) => a // can't express that name is *not* the specified one
        case e@DescendantStep(NamedElementStep(n), _) if name != n => e
        case e@DescendantStep(AnyElementStep, p) => e // can't express that name is *not* the specified one
        case a@DescendantStep(NamedAttributeStep(n), _) if name != n => a
        case a@DescendantStep(AnyAttributeStep, p) => a // can't express that name is *not* the specified one
      }

      (normalize(yes), normalize(no))
    }

    /** Gets the path of the (only) child of the root node (which is the last step
      * before RootPath in the path). The input path must not be RootPath itself.
      * The result is always a ChildStep(_, RootPath).
      */
    def getRootChild(path: Set[Path]): Set[Path] = {
      def getRootChildSingle(path: Path): Path = path match {
        case RootPath => throw new IllegalArgumentException("path must not be a RootPath")
        case ChildStep(last, RootPath) => path
        case DescendantStep(_, RootPath) => ChildStep(AnyElementStep, RootPath)
        case ChildStep(_, rest) => getRootChildSingle(rest)
        case DescendantStep(_, rest) => getRootChildSingle(rest)
      }

      normalize(path.map(getRootChildSingle))
    }
  }
}