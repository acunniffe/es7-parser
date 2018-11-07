package com.opticdev.parsers.es7

import com.opticdev.common.graph
import com.opticdev.common.graph.{AstGraph, AstType, CommonAstNode}
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.common.ast.{AstArray, NewAstNode}
import com.opticdev.marvin.common.helpers.InRangeImplicits._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.marvin.runtime.mutators.NodeMutatorMap
import com.opticdev.common.graph.{AstType, Child, CommonAstNode}
import com.opticdev.parsers.sourcegear.basic.{BasicSourceInterface, _}
import com.opticdev.common.graph.AstGraph
import com.opticdev.parsers.ParserBase
import play.api.libs.json._

import scala.util.{Success, Try}

class JsObjectPatternArrayLiteralInterface extends ArrayLiterals {
  override val astType: graph.AstType = AstType("ObjectPattern", "es7")

  def jsValueToNewNode(jsValue: JsValue, sourceParser: ParserBase, basicSourceInterface: BasicSourceInterface): NewAstNode = {
    import ObjectLiteralValueFormat._
    def literal(jsValue: JsValue) = NewAstNode("Literal", Map(), basicSourceInterface.tokens.generateFrom(jsValue).toOption)
    val valueWithFormat = ObjectLiteralValueFormat.valueForJson(jsValue)

    valueWithFormat.format match {
      case Primitive => {
        valueWithFormat.value match {
          case _ => literal(valueWithFormat.value)
        }
      }
    }
  }

  def extractValues(commonAstNode: CommonAstNode, raw: String, graph: AstGraph, basicSourceInterface: BasicSourceInterface) : Try[JsValue] = Try {

    commonAstNode.nodeType match {
        case AstType("Property", "es7") =>
          JsString(raw.substring(commonAstNode))
        case AstType("ObjectExpression", "es7") => basicSourceInterface.objectLiterals.parseNode(commonAstNode, graph, raw).get
        case _ => JsObject(Seq("_valueFormat" -> JsString("code"), "value" -> JsString(raw.substring(commonAstNode))))
    }

  }

  override val parser: SourceParser = (node, graph, raw, basicSourceInterface) => {
    val values = node.children(graph).filter(_._1.asInstanceOf[Child].typ == "properties")
      .map(i=> extractValues(i._2, raw, graph, basicSourceInterface))
      .collect { case Success(a) => a }
    JsArray(values)
  }

  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    implicit val nodeMutatorMap = sourceParser.marvinSourceInterface.asInstanceOf[NodeMutatorMap]
    val currentValueAsArray = parser(node, graph, raw, basicSourceInterface).as[JsArray].value
    val newValueAsArray = newValue.as[JsArray].value

    val elementsNodes = node.childrenOfType("properties")(graph).map(_._2)

    if (currentValueAsArray == newValueAsArray) raw.substring(node)
    else {

      val newArray: AstArray =
      AstArray(newValueAsArray.map(value=> {
        val currentIndex = currentValueAsArray.indexOf(value)
        if (currentIndex != -1) {
          elementsNodes(currentIndex).toMarvinAstNode(graph, raw, sourceParser)
        } else {
          jsValueToNewNode(value, sourceParser, basicSourceInterface)
        }
      }):_*)

      val asMarvinAstNode = node.toMarvinAstNode(graph, raw, sourceParser)
      asMarvinAstNode.mutator.applyChanges(asMarvinAstNode, Map("properties" -> newArray))
    }
  }
  override val generator: SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    val empty = {
      val string = "function thing({}) {}"
      val parsed = sourceParser.parseString(string)
      val graph = parsed.graph
      val possibleNodes = graph.nodes.toVector
        .filter(_.value.asInstanceOf[CommonAstNode].nodeType == astType)
      val node = possibleNodes.minBy(_.value.asInstanceOf[CommonAstNode].graphDepth(graph))
      val raw = string
      (node.value.asInstanceOf[CommonAstNode], graph, raw)
    }

    mutator(empty._1, empty._2, empty._3, newValue, sourceParser, basicSourceInterface)
  }
}
