import scala.collection.JavaConversions._
import org.jaxen.expr.{Step, Predicate}
import org.jaxen.expr.{AllNodeStep => JAllNodeStep,
                       CommentNodeStep => JCommentNodeStep,
                       ProcessingInstructionNodeStep => JProcessingInstructionNodeStep,
                       TextNodeStep => JTextNodeStep,
                       NameStep => JNameStep}

abstract class XPathStep(axis: XPathAxis, predicates: Seq[XPathExpr])
case class AllNodeStep(axis: XPathAxis, predicates: Seq[XPathExpr]) extends XPathStep(axis, predicates)
case class CommentNodeStep(axis: XPathAxis, predicates: Seq[XPathExpr]) extends XPathStep(axis, predicates)
case class ProcessingInstructionNodeStep(axis: XPathAxis, predicates: Seq[XPathExpr], name: String) extends XPathStep(axis, predicates)
case class TextNodeStep(axis: XPathAxis, predicates: Seq[XPathExpr]) extends XPathStep(axis, predicates)
case class NameStep(axis: XPathAxis, predicates: Seq[XPathExpr], name: String) extends XPathStep(axis, predicates)

object XPathStep {
  def parse(step: Step): XPathStep = {
    val axis = XPathAxis.parse(step.getAxis)
    val predicates = step.getPredicates.map(p => XPathExpr.parse(p.asInstanceOf[Predicate].getExpr))
    step match {
      // ::node()
      case allNode: JAllNodeStep => AllNodeStep(axis, predicates)
      // ::comment()
      case commentNode: JCommentNodeStep => CommentNodeStep(axis, predicates)
      // ::processing-instruction('name')
      case piNode: JProcessingInstructionNodeStep => ProcessingInstructionNodeStep(axis, predicates, piNode.getName)
      // ::text()
      case textNode: JTextNodeStep => TextNodeStep(axis, predicates)
      // any name (might also be '*')
      case nameStep: JNameStep =>
        assert(nameStep.getPrefix == null || nameStep.getPrefix.length == 0, "Prefixed names are not supported")
        NameStep(axis, predicates, nameStep.getLocalName)
    }
  }
}