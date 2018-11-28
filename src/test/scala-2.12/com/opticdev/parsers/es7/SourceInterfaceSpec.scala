package com.opticdev.parsers.es7

import com.opticdev.common.graph.{AstGraph, AstType, CommonAstNode}
import com.opticdev.parsers.sourcegear.basic._
import org.scalatest.FunSpec
import play.api.libs.json._

class SourceInterfaceSpec extends FunSpec {

  implicit val sourceParser = new OpticParser

  val jsInterface = new BasicSourceInterface {
    override val literals = LiteralInterfaces(new JsLiteralInterface)
    override val tokens = TokenInterfaces(new JsTokenInterface, new JSXTokenInterface)
    override val objectLiterals = ObjectLiteralsInterfaces(new JsObjectLiteralInterface)
    override val arrayLiterals = ArrayLiteralsInterfaces(new JsArrayLiteralInterface, new JsObjectPatternArrayLiteralInterface, new JsImportLiteralInterface)
  }

  def fixture(astType: AstType, interfaces: NodeInterfaceGroup) = new {

    def getNodeFor(string: String): (CommonAstNode, AstGraph, String) = {
      val parsed = sourceParser.parseString(string)
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

  describe("JSX Token Interface") {

    val f = fixture(AstType("JSXIdentifier", "es7"), jsInterface.tokens)

    it("can read a valid token") {
      assert(f.sourceCode("<Hello />") == JsString("Hello"))
    }

    it("can mutate a valid token") {
      assert(f.mutatedSourceCode("<Hello />", JsString("Goodbye")) == "Goodbye")
    }

    it("fails when mutating an invalid token") {
      assertThrows[Error] {
        f.mutatedSourceCode("<Hello />", JsString("bad format"))
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

      it("will give priority to the last key") {

        assert(f.sourceCode("var obj = { one: 1, one: 'one' }") == JsObject(Seq(
          "one" -> JsString("one"),
          "_order" -> JsArray(Seq(JsString("one")))
        )))

      }

      describe("value formats") {

        it("will parse non-primitives as _valueFormat = code") {
          assert(f.sourceCode("var obj = { one: 1, two: req.query.me, three: true }") == JsObject(Seq(
            "one" -> JsNumber(1),
            "two" -> JsObject(Seq(
              "_valueFormat" -> JsString("code"),
              "value" -> JsString("req.query.me")
            )),
            "three" -> JsBoolean(true),
            "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
          )))
        }

        it("will parse tokens as _valueFormat = code") {
          assert(f.sourceCode("var obj = { one: 1, two: query, three: true }") == JsObject(Seq(
            "one" -> JsNumber(1),
            "two" -> JsObject(Seq(
              "_valueFormat" -> JsString("token"),
              "value" -> JsString("query")
            )),
            "three" -> JsBoolean(true),
            "_order" -> JsArray(Seq(JsString("one"), JsString("two"), JsString("three")))
          )))
        }

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

      it("code valueformats are preserved") {
        val originalString = "var obj = { one: 1, two: 'two', code: thisIs.code(), three: true }"
        val updated = JsObject(Seq(
          "one" -> JsNumber(15),
          "code" -> JsObject(Seq("_valueFormat" -> JsString("code"), "value" -> JsString("thisIs.code()"))),
          "three" -> JsBoolean(false),
          "_order" -> JsArray(Seq(JsString("one"), JsString("code"), JsString("three")))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 15, code: thisIs.code(), three: false }")
      }

      it("nested change applied") {
        val originalString = "var obj = { one: 1, two: 'value'}"
        val updated = JsObject(Seq(
          "one" -> JsNumber(1),
          "two" -> JsObject(Seq(
            "three" -> JsBoolean(true),
            "four" -> JsBoolean(false)
          ))
        ))

        assert(f.mutatedSourceCode(originalString, updated) == "{ one: 1, two: { three: true, four: false } }")
      }

    }

  }

  describe("Js Object Pattern Array Literal Interface") {

    val f = fixture(AstType("ObjectPattern", "es7"), jsInterface.arrayLiterals)

    describe("parsing") {
      it("can parse a valid object pattern") {
        assert(f.sourceCode("function thing({a, b, c}) {}") == Json.parse("""["a", "b", "c"]"""))
      }
    }

    describe("mutating") {

      it("if nothing is changed it returns the same ") {
        val originalString = "function thing({a, b, c}) {}"
        val updated = Json.parse("""["a", "b", "c"]""")

        assert(f.mutatedSourceCode(originalString, updated) == """{a, b, c}""")
      }

      it("can append literal element") {
        val originalString = "function thing({a, b, c}) {}"
        val updated = Json.parse("""["a", "b", "c", "d"]""")

        println(f.mutatedSourceCode(originalString, updated))

        assert(f.mutatedSourceCode(originalString, updated) ==  """{ a, b, c, d }""")
      }

      it("can add literal element to the middle") {
        val originalString = "function thing({a, b, c}) {}"
        val updated = Json.parse("""["a", "d", "b", "c"]""")

        assert(f.mutatedSourceCode(originalString, updated) == """{ a, d, b, c }""")
      }

      it("can remove all elements") {
        val originalString ="function thing({a, b, c}) {}"
        val updated = Json.parse("""[]""")

        assert(f.mutatedSourceCode(originalString, updated) == "{  }")
      }

    }

  }

  describe("Js Array Literal Interface") {

    val f = fixture(AstType("ArrayExpression", "es7"), jsInterface.arrayLiterals)

    describe("parsing") {
      it("can parse a valid array with literals") {
        assert(f.sourceCode("var array = [1, 2, 'hey']") == Json.parse("""[1,2,"hey"]"""))
      }

      it("can parse a valid array with objects") {
        assert(f.sourceCode("var array = [1, 2, {'hello': 'world'}]") == Json.parse("""[1,2,{"hello":"world","_order":["hello"]}]"""))
      }

      it("can parse a valid array with sub array") {
        assert(f.sourceCode("var array = [1, 2, ['one', 'two']]") == Json.parse("""[1,2,["one", "two"]]"""))
      }

      it("can parse a valid array with tokens") {
        assert(f.sourceCode("var array = [firstToken, secondToken]") == Json.parse("""[{"_valueFormat":"token","value":"firstToken"},{"_valueFormat":"token","value":"secondToken"}]"""))
      }

      it("can parse a valid array with code") {
        assert(f.sourceCode("var array = [8*4]") == Json.parse("""[{"_valueFormat":"code","value":"8*4"}]"""))
      }
    }

    describe("mutating") {

      it("if nothing is changed it returns the same ") {
        val originalString = "var obj = [1, 2, 3]"
        val updated = Json.parse("""[1,2,3]""")

        assert(f.mutatedSourceCode(originalString, updated) == "[1, 2, 3]")
      }

      it("can append literal element") {
        val originalString = "var obj = [1,2,3]"
        val updated = Json.parse("""[1, 2, 3, 4]""")

        println(f.mutatedSourceCode(originalString, updated))

        assert(f.mutatedSourceCode(originalString, updated) == "[ 1,\n2,\n3,\n4 ]")
      }

      it("can add literal element to the middle") {
        val originalString = "var obj = [1, 2,3]"
        val updated = Json.parse("""[1, "one to two", 2, 3]""")

        assert(f.mutatedSourceCode(originalString, updated) == "[ 1,\n'one to two',\n2,\n3 ]")
      }

      it("can add array element to the middle") {
        val originalString = "var obj = [1, 2,3]"
        val updated = Json.parse("""[1, [ "test" ], 2, 3]""")

        assert(f.mutatedSourceCode(originalString, updated) == "[ 1,\n[ 'test' ],\n2,\n3 ]")
      }

      it("can add token element to the middle") {
        val originalString = "var obj = [1, 2,3]"
        val updated = Json.parse("""[1, {"_valueFormat":"token","value":"firstToken"}, 2, 3]""")

        assert(f.mutatedSourceCode(originalString, updated) == "[ 1,\nfirstToken,\n2,\n3 ]")
      }

      it("can remove all elements") {
        val originalString = "var obj = [1, 2,3]"
        val updated = Json.parse("""[]""")

        assert(f.mutatedSourceCode(originalString, updated) == "[  ]")
      }

    }

  }

  describe("Js Import Literal Interface") {

    val f = fixture(AstType("ImportDeclaration", "es7"), jsInterface.arrayLiterals)

    describe("parsing") {
      it("can parse a valid import w default") {
        assert(f.sourceCode("import me from 'there'") == Json.parse("""[{"local":"me","imported":"me","path":"there"}]"""))
      }

      it("can parse a valid named import") {
        assert(f.sourceCode("import {one} from 'there'") == Json.parse("""[{"local":"one","imported":"one","path":"there"}]"""))
      }

      it("can parse a valid named with a different local name") {
        assert(f.sourceCode("import {one as two} from 'there'") == Json.parse("""[{"local":"two","imported":"one","path":"there"}]"""))
      }

      it("can multiple items from same file") {
        assert(f.sourceCode("import {one, two, three as four} from 'there'") == Json.parse("""[{"local":"one","imported":"one","path":"there"},{"local":"two","imported":"two","path":"there"},{"local":"four","imported":"three","path":"there"}]"""))
      }
    }

    describe("generator") {
      val interface = new JsImportLiteralInterface
      def gen(jsValue: JsValue) =
        interface.generator(jsValue, sourceParser, jsInterface)
      it("can generate a default one") {
        assert(gen(Json.parse("""[{"local":"me","imported":"me","path":"there"}]""")) == """import me from 'there'""")
      }

      it("can generate single import with custom name") {
        assert(gen(Json.parse("""[{"local":"hello","imported":"me","path":"there"}]""")) == "import {me as hello} from 'there'")
      }

      it("can generate multiple imports") {
        assert(gen(Json.parse("""[{"local":"one","imported":"one","path":"there"},{"local":"two","imported":"two","path":"there"},{"local":"four","imported":"three","path":"there"}]""")) ==
          "import {one, two, three as four} from 'there'")
      }

    }

  }

}
