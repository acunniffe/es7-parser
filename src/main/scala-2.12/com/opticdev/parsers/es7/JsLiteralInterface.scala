package com.opticdev.parsers.es7

import com.opticdev.parsers.graph.{CommonAstNode, AstType}
import com.opticdev.parsers.sourcegear.basic._
import play.api.libs.json._

class JsLiteralInterface extends Literal {
  override val astType: AstType = AstType("Literal", "es7")
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    if (isRegex(node)) {
      JsString(raw)
    } else {
      node.properties \ "value" get
    }
  }

  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {

    import JsImplicits._

    val oldValue = parser(node, graph, raw, sourceParser.basicSourceInterface)
    if (oldValue == newValue) {
      raw
    } else {
      if (oldValue.getClass == newValue.getClass) {
        raw.replace(oldValue.asRawString, newValue.asRawString)
      } else {
        //if the type has changed different, we need to regenerate them
        newValue.asCodeString
      }
    }
  }

  def isRegex(node: CommonAstNode) : Boolean =  node.properties \ "regex" isDefined

  override val generator : SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    import JsImplicits._
    newValue.asCodeString
  }

}


object JsImplicits {
  implicit class CustomJsValue(jsValue: JsValue) {
    def asRawString = jsValue match {
      case JsString(s) => s
      case JsNumber(n) => n.toString()
      case JsBoolean(b) => b.toString
      case JsNull => "null"
    }

    def asCodeString = jsValue match {
      case JsNumber(n) => n.toString()
      case JsBoolean(n) => n.toString()
      case JsString(n) => "'"+n.toString()+"'"
      case JsNull => "null"
    }

  }
}