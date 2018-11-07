package com.opticdev.parsers.es7

import com.opticdev.common.graph.AstType
import com.opticdev.common.graph.GraphImplicits._
import org.scalatest.FunSpec

class EnterOnPostProcessorSpec extends FunSpec {

  val jsParser = new OpticParser

  it("will return first child for ExpressionStatements") {

    val parseResult = jsParser.parseString("req.query.name")
    implicit val astGraph = parseResult.graph
    val root = parseResult.graph.root.get

    val firstChild = root.children.head._2

    val processed = jsParser.enterOnPostProcessor.get(firstChild.nodeType).map(_.apply(firstChild.nodeType, astGraph, firstChild))

    assert(processed.isDefined)
    assert(processed.get._1 == Set(AstType("MemberExpression", "es7")))
    assert(processed.get._2.nodeType == AstType("MemberExpression", "es7"))
    null

  }

}
