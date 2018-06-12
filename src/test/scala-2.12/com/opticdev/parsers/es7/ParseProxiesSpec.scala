package com.opticdev.parsers.es7

import com.opticdev.parsers.graph.CommonAstNode
import org.scalatest.FunSpec
import com.opticdev.parsers.graph.GraphImplicits._

class ParseProxiesSpec extends FunSpec {

  val parser = new OpticParser

  describe("Case Proxy") {

    it("should use returns true when applicable") {
      assert(ParseProxies.caseProxy.shouldUse("case me: return {}", parser))
      assert(ParseProxies.caseProxy.shouldUse("      \n\n case me: return {}", parser))
      assert(ParseProxies.caseProxy.shouldUse("      \n\n case 'me+you': return {}", parser))
      assert(ParseProxies.caseProxy.shouldUse(" case 'me+you': \n 1+1 \n break", parser))
    }

    it("should use returns false when not") {
      assert(!ParseProxies.caseProxy.shouldUse("switch (thing) { \n case me: return {} \n }", parser))
      assert(!ParseProxies.caseProxy.shouldUse("CallExpression()", parser))
      assert(!ParseProxies.caseProxy.shouldUse("(case 'me+you': return {})", parser))
    }

    it("can parse by proxy") {
      val input =
        """
          |case abc: {
          |   return 1+5
          |}
        """.stripMargin

      val proxyResult = ParseProxies.caseProxy.parse(input, parser)
      assert(proxyResult.get.graph.root.get.nodeType.name == "Program")
      assert(proxyResult.get.graph.root.get.dependents(proxyResult.get.graph).head.asInstanceOf[CommonAstNode].nodeType.name == "SwitchCase")
      assert(proxyResult.get.graph.toString == "Graph(CommonAstNode(AstType(BinaryExpression,es7),Range 22 until 25,{\"operator\":\"+\"},SPACE), CommonAstNode(AstType(BlockStatement,es7),Range 10 until 27,{},SPACE), CommonAstNode(AstType(Identifier,es7),Range 5 until 8,{\"name\":\"abc\"},SPACE), CommonAstNode(AstType(Literal,es7),Range 22 until 23,{\"value\":1},SPACE), CommonAstNode(AstType(Literal,es7),Range 24 until 25,{\"value\":5},SPACE), CommonAstNode(AstType(Program,es7),Range 0 until 27,{\"sourceType\":\"module\"},SPACE), CommonAstNode(AstType(ReturnStatement,es7),Range 15 until 25,{},SPACE), CommonAstNode(AstType(SwitchCase,es7),Range 0 until 27,{},SPACE), CommonAstNode(AstType(BinaryExpression,es7),Range 22 until 25,{\"operator\":\"+\"},SPACE)~>CommonAstNode(AstType(Literal,es7),Range 22 until 23,{\"value\":1},SPACE) 'Child(0,left,false), CommonAstNode(AstType(BinaryExpression,es7),Range 22 until 25,{\"operator\":\"+\"},SPACE)~>CommonAstNode(AstType(Literal,es7),Range 24 until 25,{\"value\":5},SPACE) 'Child(0,right,false), CommonAstNode(AstType(BlockStatement,es7),Range 10 until 27,{},SPACE)~>CommonAstNode(AstType(ReturnStatement,es7),Range 15 until 25,{},SPACE) 'Child(0,body,true), CommonAstNode(AstType(Program,es7),Range 0 until 27,{\"sourceType\":\"module\"},SPACE)~>CommonAstNode(AstType(SwitchCase,es7),Range 0 until 27,{},SPACE) 'Child(0,children,true), CommonAstNode(AstType(ReturnStatement,es7),Range 15 until 25,{},SPACE)~>CommonAstNode(AstType(BinaryExpression,es7),Range 22 until 25,{\"operator\":\"+\"},SPACE) 'Child(0,argument,false), CommonAstNode(AstType(SwitchCase,es7),Range 0 until 27,{},SPACE)~>CommonAstNode(AstType(BlockStatement,es7),Range 10 until 27,{},SPACE) 'Child(0,consequent,true), CommonAstNode(AstType(SwitchCase,es7),Range 0 until 27,{},SPACE)~>CommonAstNode(AstType(Identifier,es7),Range 5 until 8,{\"name\":\"abc\"},SPACE) 'Child(0,test,false))")
    }

    it("fails if syntax is invalid") {
      val input =
        """
          |case (): {
          |   +++++ssa= fjkhsdf jkhsd
          |}
        """.stripMargin

      assert(ParseProxies.caseProxy.parse(input, parser).isFailure)
    }

  }

}
