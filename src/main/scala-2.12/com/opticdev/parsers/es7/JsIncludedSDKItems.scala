package com.opticdev.parsers.es7

import com.opticdev.parsers.sdk_subset.generator.Generator
import com.opticdev.parsers.sdk_subset.{Abstraction, IncludedSDKItems}
import com.opticdev.sdk.descriptions.enums.FinderEnums.{Containing, Entire}
import com.opticdev.sdk.skills_sdk.lens.{Literal, OMLensCodeComponent, OMStringFinder, Token, ArrayLiteral}
import play.api.libs.json.{JsObject, Json}

object JsIncludedSDKItems {

  lazy val basicImportAbstraction =
    Abstraction("basic-import", Json.parse(
    """
      |{"type": "object", "properties": {
      |   "definedAs": {"type": "string"},
      |   "path": {"type": "string"}
      |}}
    """.stripMargin).as[JsObject])

  lazy val namedImportAbstraction =
    Abstraction("named-import", Json.parse(
    """
      |{"type": "object", "properties": {
      |   "imported": {"type": "string"},
      |   "local": {"type": "string"},
      |   "path": {"type": "string"}
      |}}
    """.stripMargin).as[JsObject])

  lazy val multipleImportsAbstraction =
    Abstraction("multiple-import", Json.parse(
      s"""
        |{"type": "object", "properties": {
        |   "imports": {"type": "array", "items": ${namedImportAbstraction.schema.toString()} }
        |}}
      """.stripMargin).as[JsObject])

//  lazy val basicImport =
//    Generator("basic-import-statement", "basic-import",
//    """
//      |import localName from 'path'
//    """.stripMargin,
//      Map(
//        "definedAs" -> OMLensCodeComponent(Token, OMStringFinder(Entire, "localName")),
//        "path" -> OMLensCodeComponent(Literal, OMStringFinder(Containing, "path")),
//      )
//    )

  lazy val requireImport =
    Generator("require-import-statement", "named-import",
      """
        |const local = require('path').imported
      """.stripMargin,
      Map(
        "local" -> OMLensCodeComponent(Token, OMStringFinder(Entire, "local")),
        "imported" -> OMLensCodeComponent(Token, OMStringFinder(Entire, "imported")),
        "path" -> OMLensCodeComponent(Literal, OMStringFinder(Containing, "path")),
      )
    )

  lazy val esImport =
    Generator("es-import-statement", "multiple-import",
      """
        |import local from 'path'
      """.stripMargin,
      Map(
        "imports" -> OMLensCodeComponent(ArrayLiteral, OMStringFinder(Containing, "import local from 'path'")),
      )
  )

  def all = IncludedSDKItems(basicImportAbstraction, namedImportAbstraction, multipleImportsAbstraction, requireImport, esImport)

}
