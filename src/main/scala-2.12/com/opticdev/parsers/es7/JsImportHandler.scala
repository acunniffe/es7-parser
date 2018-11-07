package com.opticdev.parsers.es7

import better.files.File
import com.opticdev.parsers.imports
import com.opticdev.parsers.imports.{AliasImport, ImportHandler, StandardImport}
import play.api.libs.json.{JsObject, JsString}

import scala.util.Try

object JsImportHandler extends ImportHandler {
  override val internalAbstractions: Seq[String] = Seq("import")

  override def importsFromModels(models: Set[imports.ImportModel])
                                (implicit inFile: File, projectDirectory: String): Map[File, Set[imports.StandardImport]] = {

    def path(jsObject: JsObject) = fileFromPath(jsObject.value("path").as[JsString].value)

    models.map {
      case m if m.abstractionId == "basic-import" => Try {
        val definedAS = m.value.value("definedAs").as[JsString].value
        AliasImport(definedAS -> definedAS, path(m.value).get)
      }
    }
    .collect{ case m if m.isSuccess => m.get}
    .groupBy(_.file)

  }

  def fileFromPath(path: String)(implicit inFile: File, projectDirectory: String): Option[File] = {
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

    if (file.hasExtension) {
      Some(file)
    } else { //only part of resolver this hits the file system
      Try(file.parent.list.find(_.nameWithoutExtension == file.name).get).toOption
    }

  }

}
