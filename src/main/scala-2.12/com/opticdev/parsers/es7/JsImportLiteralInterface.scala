package com.opticdev.parsers.es7

import com.opticdev.common.graph
import com.opticdev.common.graph.{AstGraph, AstType, Child, CommonAstNode}
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.common.ast.{AstArray, NewAstNode}
import com.opticdev.marvin.common.helpers.InRangeImplicits._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.marvin.runtime.mutators.NodeMutatorMap
import com.opticdev.parsers.ParserBase
import com.opticdev.parsers.sourcegear.basic.{BasicSourceInterface, _}
import play.api.libs.json._

import scala.util.{Success, Try}

class JsImportLiteralInterface extends ArrayLiterals {
  override val astType: graph.AstType = AstType("ImportDeclaration", "es7")


  override val parser: SourceParser = (node, graph, raw, basicSourceInterface) => {
    val specifiers = node.children(graph).filter(_._1.asInstanceOf[Child].typ == "specifiers")

    val source = {
      val so = node.childrenOfType("source")(graph).head._2
      basicSourceInterface.literals.parseNode(so, graph, raw.substring(so)).get
    }

    val jsonArrayValues = specifiers.map(i=> {
      val s = i._2

      val local = {
        val l = s.childrenOfType("local")(graph).head._2
        basicSourceInterface.tokens.parseNode(l, graph, raw.substring(l)).get
      }

      val imported = Try {
        val im = s.childrenOfType("imported")(graph).head._2
        basicSourceInterface.tokens.parseNode(im, graph, raw.substring(im)).get
      }.getOrElse(local)

      JsObject(Seq(
        "local" -> local,
        "imported" -> imported,
        "path" -> source
      ))
    })

    if (jsonArrayValues.isEmpty) {
      JsArray(Seq(JsObject(Seq(
        "path" -> source
      ))))
    } else {
      JsArray(jsonArrayValues)
    }
  }

  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    //@todo make this fancy...not needed for simple import inspection.
    generator(newValue, sourceParser, basicSourceInterface)
  }

  override val generator: SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {

    val asArray = newValue.as[JsArray]
    require(asArray.value.nonEmpty, "Import declaration can not be empty")
    val oneItem = asArray.value.size == 1

    var sourceAll: String = null
    var defaultCase = false

    val inner = asArray.value.map { i=>
      sourceAll = (i \ "path").get.as[JsString].value
      val imported = (i \ "imported").get.as[JsString].value
      val local = (i \ "local").get.as[JsString].value

      if (imported == local) {
        defaultCase = oneItem
        local
      } else {
        s"${imported} as ${local}"
      }
    }.mkString(", ")

    if (oneItem && defaultCase) {
      s"import ${inner} from '${sourceAll}'"
    } else {
      s"import {${inner}} from '${sourceAll}'"
    }
  }
}
