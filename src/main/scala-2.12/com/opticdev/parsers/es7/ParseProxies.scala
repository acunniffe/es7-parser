package com.opticdev.parsers.es7

import com.opticdev.parsers.graph.{AstType, Child, CommonAstNode}
import com.opticdev.parsers.{AstGraph, ParserBase, ParserResult}
import com.opticdev.parsers.sourcegear.ParseProxy
import com.opticdev.parsers.graph.GraphImplicits._
import scalax.collection.mutable.Graph
import scalax.collection.edge.Implicits._

import scala.util.Try

object ParseProxies {

  val caseProxy = new ParseProxy {

    val caseRegex = "^case .+:".r

    private def placeInWrapper(input: String): String =
      s"""function anon() {
         |switch (1) {
         |${input}
         |}
         |}
       """.stripMargin

    override def shouldUse(input: String, parser: ParserBase): Boolean = {
      caseRegex.findFirstMatchIn(input.trim).isDefined
    }
    override def parse(input: String, parser: ParserBase): Try[ParserResult] = Try {
      val newInput = placeInWrapper(input)

      val result = parser.parseString(newInput)

      val caseNode = result.graph.nodes.collect {
        case n if n.isASTType(AstType("SwitchCase", parser.languageName)) =>
          n.value.asInstanceOf[CommonAstNode]
      }.minBy(_.graphDepth(result.graph))


      val repositioned = result.graph.repositionGraphHead(caseNode, newInput)

      result.copy(graph = repositioned._3)
    }
  }

}
