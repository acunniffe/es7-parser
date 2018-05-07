package com.opticdev.parsers.es7

import com.opticdev.parsers.{AstGraph, ParserBase}
import com.opticdev.parsers.graph.{CommonAstNode, AstType, Child, GraphImplicits}
import com.opticdev.parsers.sourcegear.basic._
import play.api.libs.json._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.marvin.common.helpers.InRangeImplicits._

import scala.util.{Failure, Success, Try}
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.runtime.mutators.NodeMutatorMap

import scala.collection.immutable

class JsObjectLiteralInterface extends ObjectLiterals {
  override val astType: AstType = AstType("ObjectExpression", "es7")

  def extractKey(CommonAstNode: CommonAstNode, raw: String, graph: AstGraph, basicSourceInterface: BasicSourceInterface) : Try[String] = Try {
    val children = CommonAstNode.children(graph)
    val key = children.find(_._1.asInstanceOf[Child].typ == "key").map(i=> {
      i._2.nodeType match {
        case AstType("Literal", "es7") => (i._2.properties \ "value").get.as[JsString].value
        case AstType("Identifier", "es7") => (i._2.properties \ "name").get.as[JsString].value
      }
    })
    key.get
  }

  def extractKeyValuePairs(CommonAstNode: CommonAstNode, raw: String, graph: AstGraph, basicSourceInterface: BasicSourceInterface) : Try[(String, JsValue)] = Try {
      val children = CommonAstNode.children(graph)
      val key = extractKey(CommonAstNode, raw, graph, basicSourceInterface)

      val value : Option[JsValue] = children.find(_._1.asInstanceOf[Child].typ == "value").map(i=> {
        i._2.nodeType match {
          case AstType("Literal", "es7") =>
            basicSourceInterface.literals.parseNode(i._2, graph, raw.substring(i._2))
              .get
          case AstType("Identifier", "es7") =>
            JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> basicSourceInterface.tokens.parseNode(i._2, graph, raw.substring(i._2))
              .get))
          case AstType("ObjectExpression", "es7") => basicSourceInterface.objectLiterals.parseNode(i._2, graph, raw).get
          case _ => JsObject(Seq("_valueFormat" -> JsString("code"), "value" -> JsString(raw.substring(i._2))))
        }
      })

      (key.get, value.get)
  }

  def jsValueToNewNode(jsValue: JsValue, sourceParser: ParserBase, basicSourceInterface: BasicSourceInterface): NewAstNode = {
    import ObjectLiteralValueFormat._
    def literal(jsValue: JsValue) = NewAstNode("Literal", Map(), basicSourceInterface.literals.generateFrom(jsValue).toOption)
    val valueWithFormat = ObjectLiteralValueFormat.valueForJson(jsValue)

    valueWithFormat.format match {
      case Primitive => {
        valueWithFormat.value match {
          case obj: JsObject => NewAstNode("ObjectExpression", Map(), Some(generator(obj, sourceParser, basicSourceInterface)))
          case _ => literal(valueWithFormat.value)
        }
      }
      case Token => NewAstNode("Identifier", Map(), Some(valueWithFormat.value.as[JsString].value))
      case Code => NewAstNode("Expression", Map(), Some(valueWithFormat.value.as[JsString].value))
    }
  }


  def acceptProperties(CommonAstNode: CommonAstNode) = {
    (CommonAstNode.properties \ "kind").get.as[JsString].value == "init" &&
      !(CommonAstNode.properties \ "computed").get.as[JsBoolean].value &&
      CommonAstNode.nodeType.name == "Property"
  }

  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {

    val propertiesKeyValuePairs = node.children(graph).filter(_._1.asInstanceOf[Child].typ == "properties")
      .filter(i=> acceptProperties(i._2))
      .map(i=> extractKeyValuePairs(i._2, raw, graph, basicSourceInterface))
      .collect { case Success(a) => a }

    JsObject(propertiesKeyValuePairs :+ ("_order" -> JsArray(propertiesKeyValuePairs.map(_._1).distinct.map(JsString))))
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    implicit val nodeMutatorMap = sourceParser.marvinSourceInterface.asInstanceOf[NodeMutatorMap]

    val asObject = newValue.as[JsObject]
    val orderArray = Try(
      (asObject \ "_order").as[JsArray].value.toVector.map(_.as[JsString].value)
    ).getOrElse(Vector())


    val properties = node.children(graph).filter(_._1.asInstanceOf[Child].typ == "properties")
    val propertiesAstNodes = properties
      .filter(i=> acceptProperties(i._2) && extractKeyValuePairs(i._2, raw, graph, basicSourceInterface).isSuccess)
      .map(_._2)

    val unsupportedNodes: Seq[(String, Int, CommonAstNode)] = properties
      .filter(i=> acceptProperties(i._2) && extractKeyValuePairs(i._2, raw, graph, basicSourceInterface).isFailure)
      .map(i=> (extractKey(i._2, raw, graph, basicSourceInterface).get, i._1.asInstanceOf[Child].index, i._2))
      //removes any unsupported nodes that are supported later on
      .filterNot(i=> asObject.fields.exists(_._1 == i._1))

    val currentFieldsRaw = parser(node, graph, raw, basicSourceInterface).as[JsObject].fields
    val currentFieldsSeq = currentFieldsRaw.filterNot(_._1 == "_order")
    val currentFields = currentFieldsSeq.toSet
    val currentOrder = Try(
      currentFieldsRaw.find(_._1 == "_order").map(_._2.as[JsArray].value.toVector.map(_.as[JsString].value)).get)
      .getOrElse(Vector())
    val newFields = asObject.fields.filterNot(_._1 == "_order").toSet

    if (currentFields == newFields && currentOrder == orderArray) raw.substring(node)
    else {

      val sameKeySameValue = currentFields intersect newFields

      val sameKeyDifferentValue = newFields.filter(i=> !sameKeySameValue.exists(_._1 == i._1) && currentFields.exists(_._1 == i._1))

      val newKeys = newFields.filterNot(i=> currentFields.exists(_._1 == i._1))

      val propertyMutator = nodeMutatorMap.mapping("Property")

      val astProperties: Seq[(String, BaseAstNode)] = {
        sameKeySameValue.toSeq.map(i=> {
          val index = currentFieldsSeq.indexWhere(_._1 == i._1)
          (i._1, propertiesAstNodes(index).toMarvinAstNode(graph, raw, sourceParser))
        }) ++
        sameKeyDifferentValue.toSeq.map(i=> {
          val key = i._1
          val index = currentFieldsSeq.indexWhere(_._1 == i._1)
          val propertyNode = propertiesAstNodes(index).toMarvinAstNode(graph, raw, sourceParser)

          val currentValue = currentFields.find(_._1 == key).get
          val newValue = newFields.find(_._1 == key).get

          //data has same type
          val newValueAst = if (currentValue._2.getClass == newValue._2.getClass) {
            //@todo use the source interface here.
            jsValueToNewNode(newValue._2, sourceParser, basicSourceInterface)
          } else {
            jsValueToNewNode(newValue._2, sourceParser, basicSourceInterface)
          }

          val updated = propertyMutator.applyChanges(propertyNode, Map("value" -> newValueAst))
          (i._1, NewAstNode("Property", Map(), Some(updated)))

        }) ++
        newKeys.toSeq.map(i=> {
          //try to make it an identifier first, otherwise add quotes and declare it as a literal
          val key = basicSourceInterface.tokens.generateFrom(JsString(i._1))
              .map(i=> NewAstNode("Identifier", Map(), Some(i)))
              .getOrElse(NewAstNode("Literal", Map(), Some(basicSourceInterface.literals.generateFrom(JsString(i._1)).get)))

          val value = jsValueToNewNode(i._2, sourceParser, basicSourceInterface)

          (i._1, NewAstNode("Property", Map(), Some(propertyMutator.generate(Map(
            "key" -> key,
            "value" -> value
          )))))
        })
      }

      val sortedAstProperties = astProperties.sortBy(i=> {
        val index = orderArray.indexOf(i._1)
        if (index == -1) Int.MaxValue else index
      }).map(_._2)


      val sortedWithUnsupportedNodes = unsupportedNodes.foldLeft(sortedAstProperties) {
        case (sorted, (key, index, node)) => {
          val asAstNode = node.toMarvinAstNode(graph, raw, sourceParser)
          sorted.patch(index, Seq(asAstNode), 0)
        }
      }

      val asMarvinAstNode = node.toMarvinAstNode(graph, raw, sourceParser)
      asMarvinAstNode.mutator.applyChanges(asMarvinAstNode, Map("properties" -> AstArray(sortedWithUnsupportedNodes:_*)))
    }
  }
  override val generator : SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    ""
  }
}