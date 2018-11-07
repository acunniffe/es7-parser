package com.opticdev.parsers.es7

import com.opticdev.common.graph
import com.opticdev.marvin.common.ast.{AstArray, AstNode, BaseAstNode, NewAstNode}
import com.opticdev.common.graph.AstGraph
import com.opticdev.common.graph.{AstType, Child, CommonAstNode}
import com.opticdev.parsers.sourcegear.basic.{BasicSourceInterface, _}
import play.api.libs.json._
import com.opticdev.marvin.common.helpers.InRangeImplicits._
import com.opticdev.marvin.runtime.mutators.NodeMutatorMap
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.parsers.ParserBase

import scala.util.{Success, Try}

class JsArrayLiteralInterface extends ArrayLiterals {
  override val astType: graph.AstType = AstType("ArrayExpression", "es7")

  def jsValueToNewNode(jsValue: JsValue, sourceParser: ParserBase, basicSourceInterface: BasicSourceInterface): NewAstNode = {
    import ObjectLiteralValueFormat._
    def literal(jsValue: JsValue) = NewAstNode("Literal", Map(), basicSourceInterface.literals.generateFrom(jsValue).toOption)
    val valueWithFormat = ObjectLiteralValueFormat.valueForJson(jsValue)

    valueWithFormat.format match {
      case Primitive => {
        valueWithFormat.value match {
          case obj: JsObject => NewAstNode("ObjectExpression", Map(), Some(generator(obj, sourceParser, basicSourceInterface)))
          case arr: JsArray => NewAstNode("ArrayExpression", Map(), Some(generator(arr, sourceParser, basicSourceInterface)))
          case _ => literal(valueWithFormat.value)
        }
      }
      case Token => NewAstNode("Identifier", Map(), Some(valueWithFormat.value.as[JsString].value))
      case Code => NewAstNode("Expression", Map(), Some(valueWithFormat.value.as[JsString].value))
    }
  }

  def extractValues(commonAstNode: CommonAstNode, raw: String, graph: AstGraph, basicSourceInterface: BasicSourceInterface) : Try[JsValue] = Try {

    commonAstNode.nodeType match {
        case AstType("Literal", "es7") =>
          basicSourceInterface.literals.parseNode(commonAstNode, graph, raw.substring(commonAstNode)).get
        case AstType("Identifier", "es7") =>
          JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> basicSourceInterface.tokens.parseNode(commonAstNode, graph, raw.substring(commonAstNode)).get))
        case AstType("ObjectExpression", "es7") => basicSourceInterface.objectLiterals.parseNode(commonAstNode, graph, raw).get
        case AstType("ArrayExpression", "es7") => basicSourceInterface.arrayLiterals.parseNode(commonAstNode, graph, raw).get
        case _ => JsObject(Seq("_valueFormat" -> JsString("code"), "value" -> JsString(raw.substring(commonAstNode))))
    }

  }

  override val parser: SourceParser = (node, graph, raw, basicSourceInterface) => {
    val values = node.children(graph).filter(_._1.asInstanceOf[Child].typ == "elements")
      .map(i=> extractValues(i._2, raw, graph, basicSourceInterface))
      .collect { case Success(a) => a }
    JsArray(values)
  }

  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    implicit val nodeMutatorMap = sourceParser.marvinSourceInterface.asInstanceOf[NodeMutatorMap]
    val currentValueAsArray = parser(node, graph, raw, basicSourceInterface).as[JsArray].value
    val newValueAsArray = newValue.as[JsArray].value

    val elementsNodes = node.childrenOfType("elements")(graph).map(_._2)

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
      asMarvinAstNode.mutator.applyChanges(asMarvinAstNode, Map("elements" -> newArray))
    }


  }
  override val generator: SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    val empty = {
      val string = "var a = []"
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
