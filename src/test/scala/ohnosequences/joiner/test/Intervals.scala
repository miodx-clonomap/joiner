package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.joiner._, intervals._

class Intervals extends FunSuite {

  test("Interval Intersection") {

    val i1 = interval(12,25)
    val i2 = interval(0,12)
    val i3 = interval(5,41)

    assert( intersection(i1,i2)   == intervals.Empty  )
    assert( intersection(i3,i2)   == interval(5,12)   )
    assert( intersection(i1, i3)  == i1               )
  }

  test("Interval overlaps") {

    val i1 = interval(0,330)
    val i2 = interval(0,270)

    val i1_i2 = overlapsWithLength(i2.length)(i1)

    assert( i1_i2.length == 330)
    assert( i1_i2 contains ((Empty, i2, i2, Empty)) )
    assert( i1_i2 contains ((interval(0,100), interval(100,330), interval(0, 330 - 100), interval(330 - 100, 270))) )
  }
}
