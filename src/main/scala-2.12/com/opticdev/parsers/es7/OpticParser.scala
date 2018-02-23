package com.opticdev.parsers.es7

import java.io.File
import java.security.MessageDigest
import javax.script.{ScriptEngine, ScriptEngineManager}

import com.opticdev.parsers.graph.AstType
import com.opticdev.parsers.sourcegear.advanced.{BaseAstMutator, MarvinSourceInterface}
import com.opticdev.parsers.sourcegear.basic.{BasicSourceInterface, LiteralInterfaces, ObjectLiteralsInterfaces, TokenInterfaces}
import com.opticdev.parsers.utils.Profiling
import com.opticdev.parsers._
import jdk.nashorn.api.scripting.{NashornScriptEngine, ScriptObjectMirror}
import play.api.libs.json._
import javax.script.CompiledScript

import scala.io.Source
import scala.util.Random
import java.io.BufferedReader
import java.io.InputStreamReader

class OpticParser extends ParserBase {
  lazy val languageName = "es7"
  lazy val parserVersion = "0.1.0"
  lazy val fileExtensions = Set(".js")
  lazy val programNodeType = AstType("Program", languageName)
  lazy val blockNodeTypes = BlockNodeTypes(
    BlockNodeDesc(AstType("BlockStatement", languageName), "body"),
    BlockNodeDesc(AstType("Program", languageName), "body")
  )
  lazy val identifierNodeDesc = IdentifierNodeDesc(AstType("Identifier", languageName), Seq("name"))

  private val thisParser = this
  val basicSourceInterface = new BasicSourceInterface {
    override val literals = LiteralInterfaces(new JsLiteralInterface)(this, thisParser)
    override val tokens = TokenInterfaces(new JsTokenInterface)(this, thisParser)
    override val objectLiterals = ObjectLiteralsInterfaces(new JsObjectLiteralInterface)(this, thisParser)
  }

  lazy val marvinSourceInterface = JsSourceInterface

  private lazy val (engine, compiledScript) : (NashornScriptEngine, CompiledScript) = {
    val path = this.getClass.getClassLoader.getResource("acorn.js")
    val engine: NashornScriptEngine = new ScriptEngineManager(null).getEngineByName("nashorn").asInstanceOf[NashornScriptEngine]

    val acornSource = scala.io.Source.fromInputStream(path.openStream()).mkString

    val script = "(function () { \n" +
      acornSource + "\n " +
      "return {ast: JSON.stringify(acorn.parse(contents, {sourceType: 'module', ecmaVersion: 7} ))} \n"+
      "})()"

    val compiledScript = engine.compile(script)
    (engine, compiledScript)
  }

  override def parseString(contents: String): ParserResult = {

    val bindings = engine.createBindings
    bindings.put("contents", contents)

    val parsedJsonString = Profiling.time(compiledScript.eval(bindings).asInstanceOf[ScriptObjectMirror])

    val astJSON: JsValue = Json.parse(parsedJsonString.result.get("ast").asInstanceOf[String])
    val asGraph = ASTJsonToGraph.buildGraphFromJson(astJSON.asInstanceOf[JsObject])

    ParserResult(asGraph, languageName, parsedJsonString.elapsedTime)
  }
}