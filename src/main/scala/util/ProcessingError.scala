package util

/** An error that can be thrown when the processing of an XSLT template or an XPath expression fails */
class ProcessingError(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
