package com.opticdev.parsers.es7

import com.opticdev.parsers.AstGraph
import com.opticdev.parsers.graph.{AstType, CommonAstNode}
import com.opticdev.parsers.sourcegear.basic._
import org.scalatest.FunSpec
import play.api.libs.json._

class SourceInterfaceTest extends FunSpec {

  val jsParser = new OpticParser

  val jsInterface = new BasicSourceInterface {
    override val literals = LiteralInterfaces(new JsLiteralInterface)(this, jsParser)
    override val tokens = TokenInterfaces(new JsTokenInterface)(this, jsParser)
    override val objectLiterals = ObjectLiteralsInterfaces(new JsObjectLiteralInterface)(this, jsParser)
  }

  def fixture(astType: AstType, interfaces: NodeInterfaceGroup) = new {

    def getNodeFor(string: String): (CommonAstNode, AstGraph, String) = {
      val parsed = jsParser.parseString(string)
      val graph = parsed.graph
      val possibleNodes = graph.nodes.toVector
        .filter(_.value.asInstanceOf[CommonAstNode].nodeType == astType)
      val node = possibleNodes.minBy(_.value.asInstanceOf[CommonAstNode].graphDepth(graph))
      val raw = string
      (node.value.asInstanceOf[CommonAstNode], graph, raw)
    }

    def sourceCode(string: String): JsValue = {
      val parsed = getNodeFor(string)
      interfaces.parseNode(parsed._1, parsed._2, parsed._3).get
    }

    def mutatedSourceCode(string: String, newValue: JsValue) = {
      val parsed = getNodeFor(string)
      interfaces.mutateNode(parsed._1, parsed._2, string, newValue).get
    }

    def generateFrom(newValue: JsValue) = {
      interfaces.generateFrom(newValue).get
    }

  }


  describe("Js Literal Interface") {

    val f = fixture(AstType("Literal", "es7"), jsInterface.literals)
    def objectWithValue(jsValue: JsValue) = JsObject(Seq("value" -> jsValue))

    describe("Number Literals") {
      it("Can parse") {
        assert(f.sourceCode("15") == JsNumber(15))
      }
      it("Can be mutated") {
        assert(f.mutatedSourceCode("15", JsNumber(121)) == "121")
        assert(f.mutatedSourceCode("15", JsNumber(13.9)) == "13.9")
      }
    }

    describe("String Literals") {

      describe("with single quotes") {
        it("Can parse") {
          assert(f.sourceCode("'hello world'") == JsString("hello world"))
        }
        it("Can be mutated") {
          assert(f.mutatedSourceCode("'hello world'", JsString("hello mars")) == "'hello mars'")
        }
      }

      describe("with special chars") {
        it("Can parse") {
          assert(f.sourceCode("'hello[ $] \" wor*ld'") == JsString("hello[ $] \" wor*ld"))
        }
        it("Can be mutated") {
          assert(f.mutatedSourceCode("'hello[ $] \" wor*ld'", JsString("hello mars[\"*]")) == "'hello mars[\"*]'")
        }
      }

      describe("with double quotes") {
        it("Can parse") {
          assert(f.sourceCode("\"hello world\"") == JsString("hello world"))
        }
        it("Can be mutated") {
          assert(f.mutatedSourceCode("\"hello world\"", JsString("hello mars")) == "\"hello mars\"")
        }
      }

      it("other type can be mutated into single quoted string") {
        assert(f.mutatedSourceCode("12", JsString("twelve")) == "'twelve'")
      }

    }

    describe("Boolean Literals") {
      it("Can parse") {
        assert(f.sourceCode("true") == JsBoolean(true))
      }
      it("Can be mutated") {
        assert(f.mutatedSourceCode("true", JsBoolean(true)) == "true")
        assert(f.mutatedSourceCode("true", JsBoolean(false)) == "false")
      }
    }

    describe("Regex Literals") {
      it("Can parse") {
        assert(f.sourceCode("/regex/g") == JsString("/regex/g"))
        assert(f.sourceCode("/regex/") == JsString("/regex/"))
      }

      it("Can be mutated") {
        assert(f.mutatedSourceCode("/regex/g", JsString("/regex2/g")) == "/regex2/g")
      }

    }

    describe("Null Literals") {
      it("Can parse") {
        assert(f.sourceCode("null") == JsNull)
      }

      it("Can be mutated") {
        assert(f.mutatedSourceCode("15", JsNull) == "null")
      }
    }

    it("can generate a literal") {
      assert(f.generateFrom(JsString("HELLO")) == "'HELLO'")
    }

  }

  describe("Js Token Interface") {

    val f = fixture(AstType("Identifier", "es7"), jsInterface.tokens)

    it("can read a valid token") {
      assert(f.sourceCode("hello") == JsString("hello"))
    }

    it("can mutate a valid token") {
      assert(f.mutatedSourceCode("hello", JsString("goodbye")) == "goodbye")
    }

    it("fails when mutating an invalid token") {
      assertThrows[Error] {
        f.mutatedSourceCode("hello", JsString("bad format"))
      }
    }

    it("can generate a token") {
      assert(f.generateFrom(JsString("HELLO")) == "HELLO")
    }

  }


  describe("Js Object Literal Interface") {

    val f = fixture(AstType("ObjectExpression", "es7"), jsInterface.objectLiterals)

    describe("parsing") {

      it("can parse a valid object") {

        assert(f.sourceCode("var obj = { one: 1, two: 'two', three: true }") == JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(true),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        )))

      }

      it("can parse a valid object with string keys") {

        assert(f.sourceCode("var obj = { 'one': 1, 'two': 'two', 'three': true }") == JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(true),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        )))

      }

      it("can parse a nested object") {

        assert(f.sourceCode("var obj = { one: 1, two: { three: true } }") == JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsObject(Seq(
            "three" -> JsBoolean(true),
            "_order" -> JsArray(Seq(JsString("three")))
          )),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two")))
        )))

      }

      it("will parse identifiers as objects") {

        assert(f.sourceCode("var obj = { one: one, two: two, three: three }") == JsObject(Seq(
          "one" -> JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> JsString("one"))),
          "two" -> JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> JsString("two"))),
          "three" -> JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> JsString("three"))),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        )))

      }

      it("will not include invalid keys (functions), but will parse valid ones") {

        assert(f.sourceCode("var obj = { one: 1, two: ()=> {}, three: true }") == JsObject(Seq(
          "one" -> JsNumber(1),
          "three" -> JsBoolean(true),
          "_order" -> JsArray(Seq(JsString("one"), JsString("three")))
        )))

      }

      it("will give priority to the last key") {

        assert(f.sourceCode("var obj = { one: 1, one: 'one' }") == JsObject(Seq(
          "one" -> JsString("one"),
          "_order" -> JsArray(Seq(JsString("one")))
        )))

      }
    }

    describe("mutating") {

      it("if nothing is changed it returns the same ") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(true),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: 'two', three: true }")
      }

      it("will create tokens values if _valueFormat = 'token'") {
        val originalString = "var obj = { one: 1, two: 'two' }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsObject(Seq("_valueFormat" -> JsString("token"), "value" -> JsString("TESTING"))),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: 'two', three: TESTING }")
      }

      it("will create tokens values if _valueFormat = 'code'") {
        val originalString = "var obj = { one: 1, two: 'two' }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsObject(Seq("_valueFormat" -> JsString("code"), "value" -> JsString("request.query.thing"))),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: 'two', three: request.query.thing }")
      }

      it("new keys can be added") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject(Seq(
          "hello" -> JsString("world"),
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(true),
          "goodbye" -> JsString("world"),
          "_order" -> JsArray(Seq(JsString("hello"), JsString("one"), JsString("two"), JsString("three"), JsString("goodbye")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ hello: 'world', one: 1, two: 'two', three: true, goodbye: 'world' }")
      }

      it("it will return empty expression if all fields removed") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject.empty

        assert(f.mutatedSourceCode(originalString, updated) == "{  }")
      }

      it("an expression with the same order of properties + 1 at the end") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(3),
          "two" -> JsString("six"),
          "three" -> JsBoolean(true),
          "four" -> JsString("FOUR"),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 3, two: 'six', three: true, four: 'FOUR' }")
      }

      it("order of keys can be changed") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(true),
          "_order" -> JsArray(Seq(JsString("three"), JsString("two"), JsString("one")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ three: true, two: 'two', one: 1 }")
      }

      it("duplicate keys in source node don't break mutations") {
        val originalString = "var obj = { one: 1, two: 'two', three: true, three: 15 }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(false),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: 'two', three: false }")
      }

      it("duplicate keys in update don't break mutations") {
        val originalString = "var obj = { one: 1, two: 'two', three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsString("two"),
          "three" -> JsBoolean(false),
          "three" -> JsNumber(12),
          "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: 'two', three: 12 }")
      }

      it("unsupported values are preserved") {
        val originalString = "var obj = { one: 1, two: 'two', notgonnawork: ()=> {}, three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(15),
          "three" -> JsBoolean(false),
          "_order" -> JsArray(Seq(JsString("one"), JsString("three")))
        ))
        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 15, three: false, notgonnawork: ()=> {} }")
      }

    }

  }

}
