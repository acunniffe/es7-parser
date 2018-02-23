package com.opticdev.parsers.es7

object Test {

  def main(args: Array[String]): Unit = {

    val here = new OpticParser().parseString("var hello = 3+5")

    println(here)

  }

}
