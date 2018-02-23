package com.opticdev.parsers.es7

import com.opticdev.parsers.AstGraph
import com.opticdev.parsers.graph.{CommonAstNode, AstType, BuilderPhase, GraphBuilder}
import play.api.libs.json._

object ASTJsonToGraph {

  private val reservedKeysSeq : Seq[String] = Seq("type", "start", "end")
  private val reservedKeys : Set[String] = reservedKeysSeq.toSet

  def buildGraphFromJson(jsObject: JsObject) : AstGraph = {
    val graphBuilder = new GraphBuilder[CommonAstNode]()
    createASTNode(jsObject, "root", 0, graphBuilder.rootPhase)
    graphBuilder.graph
  }

  private def createASTNode(json: JsObject, childType: String, index: Int, builderPhase: BuilderPhase[CommonAstNode], fromArray : Boolean = false) : CommonAstNode = {

    val ks = keyset(json)
    if (!isNode(ks)) return null

    val nodeFeatures = json.fields.filter(i=> reservedKeys.contains(i._1))

    val propertyFields = json.fields.filter({
      case (key, value) => ks.diff(reservedKeys).contains(key)
    })

    val typee = nodeFeatures.find(_._1 == "type").get._2.asInstanceOf[JsString].value
    val start = nodeFeatures.find(_._1 == "start").get._2.asInstanceOf[JsNumber].value.toInt
    val end   = nodeFeatures.find(_._1 == "end").get._2.asInstanceOf[JsNumber].value.toInt

    val otherFields = propertyFields.filterNot({
      case (key, value) => (value.isInstanceOf[JsArray] || value.isInstanceOf[JsObject]) && key != "regex"
    })

    val node = typee match {
      case "Literal" => CommonAstNode(AstType(typee, "es7"), Range(start, end), JsObject(otherFields.filterNot(_._1 == "raw")))
      case default => CommonAstNode(AstType(typee, "es7"), Range(start, end), JsObject(otherFields))
    }

    val childrenBuilderPhase = builderPhase.addChild(index, childType, node, fromArray)

    propertyFields.filter({
      case (key, value)=> value.isInstanceOf[JsArray]
    }).foreach({
      case (key, value) => {
        value.asInstanceOf[JsArray].value.zipWithIndex.foreach({
          e =>
            val (element, idx) = (e._1, e._2)
            createASTNode(element.asInstanceOf[JsObject], key, idx, childrenBuilderPhase, true)
        })
      }
    })

    propertyFields.filter({
      case (key, value)=> value.isInstanceOf[JsObject] && isNode(keyset(value.as[JsObject]))
    }).foreach({
      case (key, value) => createASTNode(value.asInstanceOf[JsObject], key, 0, childrenBuilderPhase, false)
    })

    node
  }

  private def keyset(json: JsObject) : Set[String] = json.fields.map(_._1).toSet

  private def isNode(ks: Set[String]) : Boolean = ks.intersect(reservedKeys) == reservedKeys

}
