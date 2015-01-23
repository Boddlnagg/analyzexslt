package xml

import scala.xml._

/** Parser that creates nodes from the scala.xml document model */
object XMLParser {
  /** Creates a node from a [[scala.xml.Node]] */
  def parse(node: Node): XMLNode = {
    node match {
      case elem: Elem =>
        if (elem.namespace != null && elem.namespace != "") throw new NotImplementedError("Prefixed names are not implemented")
        XMLElement(elem.label,
          elem.attributes.asAttrMap.map { case (name, value) => XMLAttribute(name, value) }.toList,
          elem.child.map(n => parse(n)).toList
        )
      case text: Text => XMLTextNode(text.data)
      case comment: Comment => XMLComment(comment.commentText)
      case _ => throw new NotImplementedError(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }

  /** Creates an element from a [[scala.xml.Elem]] node */
  def parseElement(elem: Elem): XMLElement = {
    parse(elem).asInstanceOf[XMLElement]
  }

  /** Creates a root node from a [[scala.xml.Elem]] node */
  def parseDocument(elem: Elem): XMLRoot = {
    XMLRoot(List(parseElement(elem)))
  }

  /** Creates a root node from a string representing an XML document */
  def parseDocument(str: String): XMLRoot = {
    parseDocument(XML.loadString(str))
  }
}
