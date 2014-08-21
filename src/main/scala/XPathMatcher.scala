object XPathMatcher {
  def matches(node: XMLNode, path: LocationPath): Boolean = {
    // NOTE: only supports location path patterns (XSLT spec section 5.2) without predciates
    // match recursively from right to left
    if (path.steps.isEmpty) {
      // an empty path is always a match, but when it is an absolute path, the current node must be the root node
      if (path.isAbsolute) node.isInstanceOf[XMLRoot] else true
    } else {
      val lastStep = path.steps.last
      val restPath = LocationPath(path.steps.dropRight(1), path.isAbsolute)
      assert(lastStep.predicates.isEmpty, "predicates in paths are currently not supported")
      lastStep match {
        // special handling of pattern '//'
        case AllNodeStep(DescendantOrSelfAxis, Nil) =>
          // does any ancestor match the rest of the path?
          node.ancestors.exists(a => matches(a, restPath))
        case _ =>  val lastStepMatches = lastStep match {
          // child::node() OR attribute::node()
          // this matches any node (regardless of axis type) according to spec section 2.3
          case AllNodeStep(ChildAxis | AttributeAxis, Nil) => true
          // child::comment()
          case CommentNodeStep(ChildAxis, Nil) => node.isInstanceOf[XMLComment]
          // child::text()
          case TextNodeStep(ChildAxis, Nil) => node.isInstanceOf[XMLTextNode]
          // child::*
          case NameStep(ChildAxis, Nil, "*") => node.isInstanceOf[XMLElement]
          // child::name
          case NameStep(ChildAxis, Nil, name) => node.isInstanceOf[XMLElement] && node.asInstanceOf[XMLElement].name == name
          // attribute::*
          case NameStep(AttributeAxis, Nil, "*") => node.isInstanceOf[XMLAttribute]
          // attribute::name
          case NameStep(AttributeAxis, Nil, name) => node.isInstanceOf[XMLAttribute] && node.asInstanceOf[XMLAttribute].name == name
          // child::processing-instruction(name?)
          // can never match because processing instructions are not implemented
          case ProcessingInstructionNodeStep(ChildAxis, Nil, _) => false
          // attribute::comment() OR attribute::processing-instruction(name?) OR attribute::text()
          // these can never match anything
          case CommentNodeStep(AttributeAxis, _)
               | ProcessingInstructionNodeStep(AttributeAxis, _, _)
               | TextNodeStep(AttributeAxis, _) => false
        }

        if (!lastStepMatches) {
          false
        } else {
          // does the parent match the rest of the path?
          matches(node.parent, restPath)
        }
      }
    }
  }
}
