package util

/** An error that can be thrown when the evaluation of an XSLT template or an XPath expression fails */
class EvaluationError(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
