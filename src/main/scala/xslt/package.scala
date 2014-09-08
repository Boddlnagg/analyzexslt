package object xslt {
  type ImportPrecedence = Int

  // the concrete values here are irrelevant as long as user-defined > built-in
  val BuiltInImportPrecedence: ImportPrecedence = -1
  val UserDefinedImportPrecedence: ImportPrecedence = 0
}