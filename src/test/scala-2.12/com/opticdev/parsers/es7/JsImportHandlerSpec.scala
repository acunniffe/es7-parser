package com.opticdev.parsers.es7

import better.files.File
import com.opticdev.parsers.imports.ImportModel
import org.scalatest.FunSpec
import play.api.libs.json.{JsObject, Json}

class JsImportHandlerSpec extends FunSpec {
  val pwd = File(".")

  implicit val projectDirectory = File("src/test/resources/test-proj-dir").pathAsString
  val exampleJs = pwd / "src" / "test" / "resources" / "test-proj-dir" / "abc" / "example.js"

  describe("can extract imports from models") {
    it("works for basic-imports") {
      val results = JsImportHandler.importsFromModels(Set(
        ImportModel("basic-import", Json.parse("""{"definedAs": "hello", "path": "../example.js"}""").as[JsObject])
      ))(exampleJs, projectDirectory)
      assert(results.head.local == "hello")
    }

    it("works for named imports") {
      val results = JsImportHandler.importsFromModels(Set(
        ImportModel("named-import", Json.parse("""{"local":"exampleHandler","path":"../example.js","imported":"exampleHandler"}""").as[JsObject])
      ))(exampleJs, projectDirectory)
      assert(results.head.local == "exampleHandler")
    }
  }

  describe("can convert js paths to File objects") {

    it("can figure out a relative path") {
      implicit val file = File("src/test/resources/test-proj-dir/abc/def/test.js")
      val resolved = JsImportHandler.fileFromPath("../example.js")
      assert(resolved.get isSamePathAs exampleJs)
    }

    it("can figure out an absolute path") {
      implicit val file = File("src/test/resources/test-proj-dir/abc/def/test.js")
      val resolved = JsImportHandler.fileFromPath(exampleJs.pathAsString)
      assert(resolved.get isSamePathAs exampleJs)
    }

    it("can figure out an absolute path with no extension") {
      implicit val file = File("src/test/resources/test-proj-dir/abc/def/test.js")
      val resolved = JsImportHandler.fileFromPath("../example")
      assert(resolved.get isSamePathAs exampleJs)
    }

    it("ignores node_modules since Optic doesn't index those") {
      implicit val file = File("src/test/resources/test-proj-dir/abc/def/test.js")
      val resolved = JsImportHandler.fileFromPath("module")
      assert(resolved.isEmpty)
    }
  }
}
