package analysis.domain.zipper.set

import scala.xml._

/** Parser that creates zipper trees from the scala.xml document model */
object XMLParser {
  /** Creates a zipper tree from a [[scala.xml.Node]] */
  def parse(node: Node): ZipperTreeNode = {
    node match {
      case elem: Elem =>
        if (elem.namespace != null && elem.namespace != "") throw new NotImplementedError("Prefixed names are not implemented")
        val children = elem.child.map(n => parse(n)).toList
        val attributes = elem.attributes.asAttrMap.map { case (name, value) => ZipperTreeNode(AttributeNode(name, value), Nil) }.toList
        ZipperTreeNode(ElementNode(elem.label), (attributes ++ children).map(wrap))
      case text: Text => ZipperTreeNode(TextNode(text.data), Nil)
      case comment: Comment => ZipperTreeNode(CommentNode(comment.commentText), Nil)
      case _ => throw new NotImplementedError(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }

  /** Creates a zipper tree with a root node from a [[scala.xml.Elem]] node */
  def parseDocument(elem: Elem): ZipperLoc = {
    ZipperLoc(wrap(ZipperTreeNode(RootNode, List(wrap(parse(elem))))), FinZipperPath(Set(TopPathSegment)))
  }

  /** Creates a zipper tree with a root node from a string representing an XML document */
  def parseDocument(str: String): ZipperLoc = {
    parseDocument(XML.loadString(str))
  }

  private def wrap(node: ZipperTreeNode): ZipperTree = FinZipperTree(Set(node))
}
