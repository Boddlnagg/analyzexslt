import org.scalatest.FunSuite

import xpath._

class XPathEvalSuite extends FunSuite {
  def eval(expr: String): XPathValue = {
    // evaluate with an empty context
    XPathEvaluator.evaluate(XPathParser.parse(expr), XPathContext(null, 0, 0, Map()))
  }

  test("Numbers") {
    assertResult(NumberValue(0)) { eval("0") }
    assertResult(NumberValue(1)) { eval("1") }
    assertResult(NumberValue(1.5)) { eval("1.5") }
    assertResult(NumberValue(3)) { eval("1.5 + 1.5") }
    assertResult(NumberValue(0)) { eval("1.5 - 1.5") }
    assertResult(NumberValue(-5)) { eval("-(2 + 3)") }
    assertResult(NumberValue(2)) { eval("4 div 2") }
    assertResult(NumberValue(1)) { eval("4 mod 3") }
  }

  test("Booleans") {
    assertResult(BooleanValue(true)) { eval("true()") }
    assertResult(BooleanValue(false)) { eval("false()") }
    assertResult(BooleanValue(true)) { eval("false() or true()") }
    assertResult(BooleanValue(false)) { eval("true() and false()") }
    assertResult(BooleanValue(false)) { eval("false() or false()") }
    assertResult(BooleanValue(true)) { eval("true() and true()") }
    assertResult(BooleanValue(false)) { eval("not(true())") }
    assertResult(BooleanValue(true)) { eval("not(false())") }
    assertResult(BooleanValue(true)) { eval("true() and not(not(false() or false() or not(false() and true())))") }
  }

  test("Number comparisons") {
    assertResult(BooleanValue(true)) { eval("0 = 0") }
    assertResult(BooleanValue(false)) { eval("0 = 1") }
    assertResult(BooleanValue(true)) { eval("1 > 0") }
    assertResult(BooleanValue(false)) { eval("1 < 0") }
    assertResult(BooleanValue(true)) { eval("1 >= 1") }
    assertResult(BooleanValue(true)) { eval("1 <= 1") }
  }
}