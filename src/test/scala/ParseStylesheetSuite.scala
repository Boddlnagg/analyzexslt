import org.scalatest.FunSuite

class ParseStylesheetSuite extends FunSuite {
  // These the two stylesheets are taken from https://en.wikipedia.org/wiki/XSLT

  test("Parse stylesheet (Wikipedia #1 simplified)") {

    val parsed = new XSLTStylesheet(TestData.WikipediaStylesheet1)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assert(parsed.matchableTemplates
      .filter {case (_, _, _, importPrecedence) => importPrecedence == XSLT.UserDefinedImportPrecedence}
      .size == 2, "There must be 2 user defined matchable templates")
  }

  test("Parse stylesheet (Wikipedia #2 simplified)") {
    val parsed = new XSLTStylesheet(TestData.WikipediaStylesheet2)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assert(parsed.matchableTemplates
                 .filter {case (_, _, _, importPrecedence) => importPrecedence == XSLT.UserDefinedImportPrecedence}
                 .size == 2, "There must be 2 user defined matchable templates")
  }
}
