package com.opticdev.parsers.es7

import com.opticdev.common.graph.AstType
import com.opticdev.parsers.sourcegear.basic.{SourceGenerator, SourceMutator, SourceParser, Token}
import play.api.libs.json.JsString

class JSXTokenInterface extends Token {
  override val astType: AstType = AstType("JSXIdentifier", "es7")

  //@todo regex is not conclusive
  override def isValidValue(value: String): Boolean = value.matches("[$_a-zA-Z0=9][a-zA-Z0-9$_]*")
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    node.properties.value("name").as[JsString]
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    generator(newValue, sourceParser, basicSourceInterface)
  }

  override val generator : SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    val value = newValue.as[JsString].value
    if (isValidValue(value)) {
      value
    } else throw new Error("Invalid JSXIdentifier format.")
  }
}