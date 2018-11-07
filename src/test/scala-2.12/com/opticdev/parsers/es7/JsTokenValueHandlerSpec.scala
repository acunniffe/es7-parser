package com.opticdev.parsers.es7

import com.opticdev.common.graph.{AstGraph, AstType, CommonAstNode}
import com.opticdev.common.graph.{AstType, CommonAstNode}
import org.scalatest.FunSpec

class JsTokenValueHandlerSpec extends FunSpec {
  val parser = new OpticParser()

  def getNodeFor(string: String, astType: AstType): (CommonAstNode, AstGraph, String) = {
    val parsed = parser.parseString(string)
    val graph = parsed.graph
    val possibleNodes = graph.nodes.toVector
      .filter(_.value.asInstanceOf[CommonAstNode].nodeType == astType)
    val node = possibleNodes.minBy(_.value.asInstanceOf[CommonAstNode].graphDepth(graph))
    val raw = string
    (node.value.asInstanceOf[CommonAstNode], graph, raw)
  }

  it("will process variable declarator") {
val a =
  """
    |const other = 1+1, myVariable = 'abc'
  """.stripMargin
    val (mockModelRoot, graph, raw) = getNodeFor(a, AstType("Literal", "es7"))

    val variableEntry = JsTokenValueHandler.evaluate(mockModelRoot, graph, null)

    assert(variableEntry.isDefined)
    assert(variableEntry.get.isInternal)
    assert(variableEntry.get.key == "myVariable")
  }

  it("will process export variable declarator") {
    val a =
      """
        |export const myVariable = 'abc'
      """.stripMargin
    val (mockModelRoot, graph, raw) = getNodeFor(a, AstType("Literal", "es7"))

    val variableEntry = JsTokenValueHandler.evaluate(mockModelRoot, graph, null)

    assert(variableEntry.isDefined)
    assert(variableEntry.get.isExternal)
    assert(variableEntry.get.key == "myVariable")
  }

}
