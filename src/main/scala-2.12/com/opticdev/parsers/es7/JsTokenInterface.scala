package com.opticdev.parsers.es7

import com.opticdev.common.graph.AstType
import com.opticdev.parsers.sourcegear.basic.{SourceGenerator, SourceMutator, SourceParser, Token}
import play.api.libs.json.JsString

class JsTokenInterface extends Token {
  override val astType: AstType = AstType("Identifier", "es7")

  //@todo regex is not conclusive
  override def isValidValue(value: String): Boolean = value.matches("[$_a-zA-Z0=9][a-zA-Z0-9$_]*")
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    JsString(raw)
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    generator(newValue, sourceParser, basicSourceInterface)
  }

  override val generator : SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    val value = newValue.as[JsString].value
    if (isValidValue(value)) {
      value
    } else throw new Error("Invalid Identifier format.")
  }
}
