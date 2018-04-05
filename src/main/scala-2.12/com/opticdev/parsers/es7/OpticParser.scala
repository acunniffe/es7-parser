package com.opticdev.parsers.es7

import java.io.File
import java.security.MessageDigest

import javax.script.{ScriptEngine, ScriptEngineManager}
import com.opticdev.parsers.graph.{AstType, CommonAstNode}
import com.opticdev.parsers.sourcegear.advanced.{BaseAstMutator, MarvinSourceInterface}
import com.opticdev.parsers.sourcegear.basic.{BasicSourceInterface, LiteralInterfaces, ObjectLiteralsInterfaces, TokenInterfaces}
import com.opticdev.parsers.utils.Profiling
import com.opticdev.parsers.{AstGraph, _}
import jdk.nashorn.api.scripting.{NashornScriptEngine, ScriptObjectMirror}
import play.api.libs.json._
import javax.script.CompiledScript

import scala.io.Source
import scala.util.Random
import java.io.BufferedReader
import java.io.InputStreamReader

class OpticParser extends ParserBase {
  def languageName = "es7"
  def parserVersion = "0.1.2"
  def fileExtensions = Set(".js")
  def programNodeType = AstType("Program", languageName)
  def blockNodeTypes = BlockNodeTypes(
    BlockNodeDesc(AstType("BlockStatement", languageName), "body"),
    BlockNodeDesc(AstType("Program", languageName), "body")
  )
  def identifierNodeDesc = IdentifierNodeDesc(AstType("Identifier", languageName), Seq("name"))

  private val thisParser = this
  val basicSourceInterface = new BasicSourceInterface {
    override val literals = LiteralInterfaces(new JsLiteralInterface)(this, thisParser)
    override val tokens = TokenInterfaces(new JsTokenInterface)(this, thisParser)
    override val objectLiterals = ObjectLiteralsInterfaces(new JsObjectLiteralInterface)(this, thisParser)
  }

  def marvinSourceInterface = JsSourceInterface

  private val (engine, compiledScript) : (NashornScriptEngine, CompiledScript) = {
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

  override def excludedPaths: Seq[String] = Seq("node_modules/")

  override def enterOnPostProcessor: Map[AstType, EnterOnPostProcessor] = Map(
    //always match the first child. statement is just a pass-through node
    AstType("ExpressionStatement", languageName) -> ((typ, graph, node)=> {
      val childNode = node.children(graph).head._2
      (Set(childNode.nodeType), childNode)
    })
  )

}