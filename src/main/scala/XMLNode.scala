import scala.xml._

abstract class XMLNode
case class XMLElement(val name: String, val attributes: Map[String, String] = Map(), val children: List[XMLNode] = List()) extends XMLNode
case class XMLTextNode(val value: String) extends XMLNode
case class XMLComment(val value: String) extends XMLNode

object XMLElement {
  def apply(elem: Elem) : XMLElement = {
    XMLNode(elem).asInstanceOf[XMLElement]
  }
}

object XMLNode {
  def apply(node: Node) : XMLNode = {
    node match {
      case elem: Elem =>
        assert(elem.namespace == null || elem.namespace == "", "Prefixed names are not supported")
        XMLElement(elem.label,
          elem.attributes.asAttrMap,
          elem.child.map(n => apply(n)).toList
        )
      case text: Text => XMLTextNode(text.data)
      case comment: Comment => XMLComment(comment.commentText)
      case _ => throw new UnsupportedOperationException(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }
}
