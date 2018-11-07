package com.opticdev.parsers.es7

import com.opticdev.parsers.sdk_subset.generator.Generator
import com.opticdev.parsers.sdk_subset.{Abstraction, IncludedSDKItems}
import com.opticdev.sdk.descriptions.enums.FinderEnums.Entire
import com.opticdev.sdk.skills_sdk.lens.{OMLensCodeComponent, OMStringFinder, Token}
import play.api.libs.json.{JsObject, Json}

object JsIncludedSDKItems {

  lazy val importAbstraction =
    Abstraction("basic-import", Json.parse(
    """
      |{"type": "object", "properties": {
      |   "definedAs": {"type": "string"},
      |   "path": {"type": "string"}
      |}}
    """.stripMargin).as[JsObject])

  lazy val basicImport =
    Generator("basic-import-statement", "import",
    """
      |import item from 'path'
    """.stripMargin,
      Map(
        "definedAs" -> OMLensCodeComponent(Token, OMStringFinder(Entire, "item"))
      )
    )


  def all = IncludedSDKItems(importAbstraction, basicImport)

}
