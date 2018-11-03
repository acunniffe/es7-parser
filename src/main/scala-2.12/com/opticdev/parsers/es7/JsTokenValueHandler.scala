package com.opticdev.parsers.es7

import com.opticdev.parsers.graph.AstType
import com.opticdev.parsers.tokenvalues._
import play.api.libs.json.JsString

import scala.util.Try

object JsTokenValueHandler extends TokenValueHandler {
  import com.opticdev.parsers.tokenvalues.TokenValuePredicates._

  override val tokenRules: Seq[TokenRule] = Seq(
    //variable declarator
    TokenRule(
      (node, astGraph) => {
        parentIs(AstType("VariableDeclarator", "es7"), "init")(node, astGraph)
      },
      (node, astGraph, modelNode) => Try {
        val isExported = node.parent(astGraph).get
            .parent(astGraph)
            .exists(parent => parentIs(AstType("ExportNamedDeclaration", "es7"), "declaration")(parent, astGraph))

        val variableDeclarator = node.parent(astGraph).get
        val identifier = variableDeclarator.childrenOfType("id")(astGraph)
        val name = identifier.head._2.properties.value("name").as[JsString].value

        if (isExported) External(name, modelNode) else Internal(name, variableDeclarator.parent(astGraph).get, modelNode)

      }
    )
  )
}
