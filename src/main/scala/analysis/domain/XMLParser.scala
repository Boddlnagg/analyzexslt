package analysis.domain

import scala.xml._

class XMLParser[N, L, V](dom: Domain[N, L, V]) {
  private val xml = dom.xmlDom
  private val xpath = dom.xpathDom

  /** Parses an [[scala.xml.Node]] and creates the representation of that node in the abstract domain (type N). */
  def parse(node: Node): N = {
    def liftList(nodes: Traversable[N]): L = nodes.foldLeft(xml.createEmptyList())((acc, next) => xml.concatLists(acc, xml.createSingletonList(next)))

    node match {
      case elem: Elem =>
        if (elem.namespace != null && elem.namespace != "") throw new NotImplementedError("Prefixed names are not implemented")
        val children = elem.child.map(n => parse(n))
        val attributes = elem.attributes.asAttrMap.map { case (name, value) => xml.createAttribute(xpath.liftString(name), xpath.liftString(value)) }
        xml.createElement(xpath.liftString(elem.label), liftList(attributes), liftList(children))
      case text: Text => xml.createTextNode(xpath.liftString(text.data))
      case comment: Comment => xml.createComment(xpath.liftString(comment.commentText))
      case _ => throw new NotImplementedError(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }

  def parseDocument(elem: Elem): N = xml.wrapInRoot(xml.createSingletonList(parse(elem)))
}
