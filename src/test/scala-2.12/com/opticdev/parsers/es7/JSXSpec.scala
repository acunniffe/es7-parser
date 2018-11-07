package com.opticdev.parsers.es7

import com.opticdev.marvin.common.ast.{AstArray, AstBoolean, AstString, NewAstNode}
import com.opticdev.common.graph.AstType
import org.scalatest.FunSpec

import scala.util.Try

class JSXSpec extends FunSpec {

  val parser = new OpticParser()

  it("can properly parse JSX") {

    val example =
      """
        |function render() {
        | return (<Node id="nodeName"> HEY </Node>)
        |}
      """.stripMargin

    val parsed = Try(parser.parseString(example))

    assert(parsed.isSuccess)
    assert(parsed.get.graph.nodes.toVector.exists(i => i.value.isASTType(AstType("JSXElement", "es7"))))
  }

  describe("AstMutators") {

    def identifier(name: String) = NewAstNode("JSXIdentifier", Map("name" -> AstString(name)))

    def stringLiteral(value: String) = NewAstNode("StringLiteral", Map("value" -> AstString(value)))

    def attribute(name: String, value: String) = NewAstNode("JSXAttribute", Map(
      "name" -> identifier(name),
      "value" -> stringLiteral(value)
    ))

    def node(name: String, selfClosing: Boolean, attributes: Seq[NewAstNode] = Seq(), children: Seq[NewAstNode] = Seq()) = {
      val n = Map(
        "openingElement" -> NewAstNode("JSXOpeningElement",
          Map(
            "name" -> NewAstNode("JSXIdentifier", Map("name" -> AstString(name))),
            "selfClosing" -> AstBoolean(selfClosing),
            "attributes" -> AstArray(attributes:_*)
          )),
        "children" -> AstArray(children:_*)
      )

      if (selfClosing) n else n ++ Map("closingElement" -> NewAstNode("JSXClosingElement",
        Map("name" -> identifier(name))))
    }

    describe("JSXElement") {
      val mutator = JsSourceInterface.mapping("JSXElement")

      it("works when self closing") {
        val code = mutator.generate(node("TestClass", true, Seq(attribute("test", "value"))))
        assert(code === "<TestClass test=\"value\"/>")
      }

      it("works when self closing and multiple attributes") {
        val code = mutator.generate(node("TestClass", true, Seq(attribute("test", "value"), attribute("other", "value"))))

        assert(code === """<TestClass
                          |test="value"
                          |other="value"
                          |/>""".stripMargin)

      }

      it("works when not self closing") {
        val code = mutator.generate(node("TestClass", false, Seq(attribute("test", "value"))))

        assert(code === "<TestClass test=\"value\"></TestClass>")
      }

      it("works when not self closing with children") {
        val code = mutator.generate(Map(
          "openingElement" -> NewAstNode("JSXOpeningElement",
            Map(
              "name" -> NewAstNode("JSXIdentifier", Map("name" -> AstString("TestClass"))),
              "selfClosing" -> AstBoolean(false),
              "attributes" -> AstArray())
          ),
          "closingElement" -> NewAstNode("JSXClosingElement",
            Map("name" -> identifier("TestClass")))
        ))

        assert(code === "<TestClass></TestClass>")
      }

      it("works more than one children") {
        val code = mutator.generate(node("TestClass", false, Seq(attribute("test", "value")), Seq(
          NewAstNode("JSXElement", node("div", true)),
          NewAstNode("JSXElement", node("div", false))
        )))

        assert(code === """<TestClass test="value">
                          |<div/>
                          |<div></div>
                          |</TestClass>""".stripMargin)

      }

      it("works more exactly one child") {
        val code = mutator.generate(node("TestClass", false, Seq(attribute("test", "value")), Seq(
          NewAstNode("JSXElement", node("div", true))
        )))

        assert(code === """<TestClass test="value"><div/></TestClass>""")

      }


    }

  }

}
