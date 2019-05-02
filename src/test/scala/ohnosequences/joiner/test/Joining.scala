package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.joiner._, DNADistributions._, intervals._, bestOverlap._

class Joining extends FunSuite {


  val A     = DNAD(A = 0.99, T = 0.005, C = 0.0025, G = 0.0025)
  val lowA  = DNAD(A = 0.7, T = 0.1, C = 0.1, G = 0.1)
  val T     = DNAD(T = 0.99, A = 0.005, C = 0.0025, G = 0.0025)

  test("conservative substitution to uniform") {

    val zz: DNASeq = Array(A,lowA,T,lowA,T)

    assert { zz.uniformOver(0.2)  === Array(A,uniform,T,uniform,T) }
    assert { zz.uniformOver(0.31) === zz }
    assert { Array(uniform,uniform,uniform).uniformOver(0.3) === Array(uniform,uniform,uniform) }
  }
}
