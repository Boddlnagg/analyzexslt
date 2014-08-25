import scala.collection.JavaConversions._
import org.jaxen.expr.{Step, Predicate}
import org.jaxen.expr.{AllNodeStep => JAllNodeStep,
                       CommentNodeStep => JCommentNodeStep,
                       ProcessingInstructionNodeStep => JProcessingInstructionNodeStep,
                       TextNodeStep => JTextNodeStep,
                       NameStep => JNameStep}

case class XPathStep(axis: XPathAxis, test: NodeTest, predicates: Seq[XPathExpr])

abstract class NodeTest
case object AllNodeTest extends NodeTest
case object CommentNodeTest extends NodeTest
case object TextNodeTest extends NodeTest
case class NameTest(name: String) extends NodeTest

object XPathStep {
  def parse(step: Step): XPathStep = {
    val axis = XPathAxis(step.getAxis)
    val predicates = step.getPredicates.map(p => XPathExpr.parse(p.asInstanceOf[Predicate].getExpr)).toList
    val nodeTest = step match {
      // ::node()
      case allNode: JAllNodeStep => AllNodeTest
      // ::comment()
      case commentNode: JCommentNodeStep => CommentNodeTest
      // ::text()
      case textNode: JTextNodeStep => TextNodeTest
      // any name (might also be '*')
      case nameStep: JNameStep =>
        assert(nameStep.getPrefix == null || nameStep.getPrefix.length == 0, "Prefixed names are not supported")
        NameTest(nameStep.getLocalName)
      // ::processing-instruction() OR ::processing-instruction('name')
      case piNode: JProcessingInstructionNodeStep => throw new NotImplementedError("Processing instructions are not implemented")
    }
    XPathStep(axis, nodeTest, predicates)
  }
}