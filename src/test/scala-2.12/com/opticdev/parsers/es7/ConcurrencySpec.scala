package com.opticdev.parsers.es7


import org.scalatest.FunSpec

class ConcurrencySpec extends FunSpec {

  it("will parse a sequence of files the same serially or in parallel") {
    val parser = new OpticParser()
    val range = Range(0, 100)

    def parse(i: Int) = parser.parseString("var number = "+i.toString)

    val serialResult = range.map(parse).toVector.map(_.graph)
    val parallelResult = range.par.map(parse).toVector.map(_.graph)
    assert(serialResult == parallelResult)

  }

}
