package parsing

import data.TestData
import org.scalatest.FunSuite
import xslt.XSLTParser

class ParseStylesheetSuite extends FunSuite {
  // These the two stylesheets are taken from https://en.wikipedia.org/wiki/XSLT

  test("Wikipedia (XSLT #1 simplified)") {

    val parsed = XSLTParser.parseStylesheet(TestData.WikipediaStylesheet1)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assertResult(2, "There must be 2 user defined matchable templates") {
      parsed.matchableTemplates
            .filter {case (_, _, _, importPrecedence) => importPrecedence == xslt.UserDefinedImportPrecedence}
            .size
    }
  }

  test("Wikipedia (XSLT #2 simplified)") {
    val parsed = XSLTParser.parseStylesheet(TestData.WikipediaStylesheet2)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assertResult(2, "There must be 2 user defined matchable templates") {
      parsed.matchableTemplates
            .filter { case (_, _, _, importPrecedence) => importPrecedence == xslt.UserDefinedImportPrecedence}
            .size
    }
  }
}
