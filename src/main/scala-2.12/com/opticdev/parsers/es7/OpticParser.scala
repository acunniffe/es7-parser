package com.opticdev.parsers.es7

import java.io.File
import java.security.MessageDigest

import javax.script.{ScriptEngine, ScriptEngineManager}
import com.opticdev.common.graph.{AstType, CommonAstNode}
import com.opticdev.parsers.sourcegear.advanced.{BaseAstMutator, MarvinSourceInterface}
import com.opticdev.parsers.sourcegear.basic._
import com.opticdev.parsers.utils.Profiling
import com.opticdev.common.graph.{AstGraph, _}
import jdk.nashorn.api.scripting.{NashornScriptEngine, ScriptObjectMirror}
import play.api.libs.json._
import javax.script.CompiledScript
import com.opticdev.sdk.rules.SameAnyOrderPlus

import scala.io.Source
import scala.util.Random
import java.io.BufferedReader
import java.io.InputStreamReader

import com.opticdev.parsers._
import com.opticdev.parsers.imports.ImportHandler
import com.opticdev.parsers.sdk_subset.IncludedSDKItems
import com.opticdev.sdk.rules.{ParserChildrenRule, SpecificChildrenRule}
import com.opticdev.parsers.sourcegear.ParseProxy
import com.opticdev.parsers.token_values.TokenValueHandler

import scala.reflect.io.File

class OpticParser extends ParserBase {
  def languageName = "es7"
  def parserVersion = "1.0.0"
  def fileExtensions = Set(".js", ".jsx")
  def programNodeType = AstType("Program", languageName)
  def blockNodeTypes = BlockNodeTypes(
    BlockNodeDesc(AstType("BlockStatement", languageName), "body"),
    BlockNodeDesc(AstType("Program", languageName), "body"),
    BlockNodeDesc(AstType("JSXElement", languageName), "children"),
    BlockNodeDesc(AstType("SwitchStatement", languageName), "cases")
//    BlockNodeDesc(AstType("SwitchStatement", languageName), "consequent")
  )
  def identifierNodeDesc = IdentifierNodeDesc(AstType("Identifier", languageName), Seq("name"))

  private implicit val sourceParser = this
  val basicSourceInterface = new BasicSourceInterface {
    override val literals = LiteralInterfaces(new JsLiteralInterface)
    override val tokens = TokenInterfaces(new JsTokenInterface)
    override val objectLiterals = ObjectLiteralsInterfaces(new JsObjectLiteralInterface)
    override val arrayLiterals: ArrayLiteralsInterfaces = ArrayLiteralsInterfaces(new JsArrayLiteralInterface, new JsObjectPatternArrayLiteralInterface, new JsImportLiteralInterface)
  }

  def marvinSourceInterface = JsSourceInterface

  private val (engine, compiledScript) : (NashornScriptEngine, CompiledScript) = {
    val acornPath = this.getClass.getClassLoader.getResource("acorn.js")
    val jsxPath = this.getClass.getClassLoader.getResource("jsxInject.js")
    val engine: NashornScriptEngine = new ScriptEngineManager(null).getEngineByName("nashorn").asInstanceOf[NashornScriptEngine]

    val acornSource = scala.io.Source.fromInputStream(acornPath.openStream()).mkString
    val jsxInject = scala.io.Source.fromInputStream(jsxPath.openStream()).mkString

    val script = s"""
        |(function () {
        |${acornSource}
        |${jsxInject}
        |return {ast: JSON.stringify(acorn.parse(contents, {sourceType: 'module', ecmaVersion: 8, 'plugins': {'jsx': true}} ))}
        |})()
      """.stripMargin

    val compiledScript = engine.compile(script)
    (engine, compiledScript)
  }

  override def parseString(contents: String): ParserResult = {

    val bindings = engine.createBindings
    bindings.put("contents", contents)

    val parsedJsonString = Profiling.time(compiledScript.eval(bindings).asInstanceOf[ScriptObjectMirror])

    val astJSON: JsValue = Json.parse(parsedJsonString.result.get("ast").asInstanceOf[String])
    val asGraph = ASTJsonToGraph.buildGraphFromJson(astJSON.asInstanceOf[JsObject])

    ParserResult(asGraph, languageName, parsedJsonString.elapsedTime, this)
  }

  override def sourcegearParseProxies: Vector[ParseProxy] = Vector(ParseProxies.caseProxy)

  override def excludedPaths: Seq[String] = Seq("node_modules/")

  override def enterOnPostProcessor: Map[AstType, EnterOnPostProcessor] = Map(
    //always match the first child. statement is just a pass-through node
    AstType("ExpressionStatement", languageName) -> ((typ, graph, node)=> {
      val childNode = node.children(graph).head._2
      (Set(childNode.nodeType), childNode)
    }),
    AstType("JSXExpressionContainer", languageName) -> ((typ, graph, node)=> {
      val childNode = node.children(graph).head._2
      (Set(childNode.nodeType), childNode)
    })
  )

  override def defaultChildrenRules: Map[AstType, Vector[ParserChildrenRule]] = Map(
    AstType("JSXOpeningElement", languageName) -> Vector(SpecificChildrenRule("attributes", SameAnyOrderPlus))
  )

  override def tokenValueHandler: TokenValueHandler = JsTokenValueHandler

  override def defaultSDKItems: IncludedSDKItems = JsIncludedSDKItems.all

  override def importHandler: ImportHandler = JsImportHandler

}