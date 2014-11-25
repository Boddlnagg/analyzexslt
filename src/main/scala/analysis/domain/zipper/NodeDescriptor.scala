package analysis.domain.zipper

abstract class NodeDescriptor
case object RootNode extends NodeDescriptor
case class ElementNode(name: String) extends NodeDescriptor
case class AttributeNode(name: String, value: String) extends NodeDescriptor
case class TextNode(value: String) extends NodeDescriptor
case class CommentNode(value: String) extends NodeDescriptor
