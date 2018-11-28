package com.opticdev.parsers.es7

import better.files.File
import com.opticdev.parsers.imports
import com.opticdev.parsers.imports.{AliasImport, ImportHandler, StandardImport}
import play.api.libs.json.{JsArray, JsObject, JsString}

import scala.util.Try

object JsImportHandler extends ImportHandler {
  override val internalAbstractions: Seq[String] = Seq("basic-import", "named-import", "multiple-import")

  override def importsFromModels(models: Set[imports.ImportModel])
                                (implicit inFile: File, projectDirectory: String, debug: Boolean = false): Set[imports.StandardImport] = {

    def path(jsObject: JsObject) = fileFromPath(jsObject.value("path").as[JsString].value)

    val imports = models.map {
      case m if m.abstractionId == "basic-import" => Try {
        val definedAS = m.value.value("definedAs").as[JsString].value
        Set(AliasImport(definedAS -> definedAS, path(m.value).get))
      }
      case m if m.abstractionId == "named-import" => Try {
        val local = m.value.value("local").as[JsString].value
        val imported = m.value.value("imported").as[JsString].value
        Set(StandardImport(local, Some(imported), path(m.value).get))
      }

      case m if m.abstractionId == "multiple-import" => Try {
        val array = (m.value \ "imports").get.as[JsArray]

        var file: Option[File] = null

        array.value.map{ i=>
          val obj = i.as[JsObject]
          val local = obj.value("local").as[JsString].value
          val imported = obj.value("imported").as[JsString].value

          if (file == null) {
            file = path(obj)
          }
          StandardImport(local, Some(imported), file.get)
        }

      }
    }

    if (debug) {
      imports.collect{ case m if m.isFailure => println(m.failed.get) }
    }

    imports.collect{ case m if m.isSuccess => m.get}.flatten
  }


  def fileFromPath(path: String)(implicit inFile: File, projectDirectory: String, debug: Boolean = false): Option[File] = {
    val file = {
      if (path.startsWith(".")) { //relative
        File(inFile.parent.pathAsString + "/" + path)
      } else {
        if (path.startsWith(projectDirectory)) {
          File(path)
        } else {
          File(projectDirectory + "/" + path)
        }
      }
    }

    //currently ignores node_modules since Optic doesn't index those
    if (file.hasExtension) {
      Some(file)
    } else { //only part of resolver this hits the file system
      Try(file.parent.list.find(_.nameWithoutExtension == file.name).get).toOption
    }

  }

}
