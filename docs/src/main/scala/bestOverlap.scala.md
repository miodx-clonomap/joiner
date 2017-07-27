
```scala
package ohnosequences.joiner

import collection.mutable.ArrayBuffer
import reflect.ClassTag
import DNADistributions._
import intervals._

case object bestOverlap {

  def whole[X](seq: Array[X]): Interval =
    interval(0, seq.length)

  def extractFrom[X:ClassTag](xs: Array[X])(i: Interval): Array[X] =
    i match {
      case Empty            => Array.empty[X]
      case LCRO(start,end)  => xs.slice(i.start, i.end)
    }

  def between(xs1: Array[DNAD], xs2: Array[DNAD]): (Array[DNAD], Double) = {

    val ovs: Array[(Interval,Interval,Interval,Interval)] =
      overlapsWithLength(xs2.length)(whole(xs1))

    var consensusScore: Double = - 1
    var consensus     : DNASeq = Array.empty[DNAD]
    var bov: (Interval,Interval,Interval,Interval) = null

    val baseErr =
      xs1.ee + xs2.ee

    var i = 0
    while(i < ovs.length) {

      val (xs1_before, xs1_shared, xs2_shared, xs2_after) =
        (extractFrom(xs1)(ovs(i)._1), extractFrom(xs1)(ovs(i)._2), extractFrom(xs2)(ovs(i)._3), extractFrom(xs2)(ovs(i)._4))

      val joinShared =
        Array(xs1_shared, xs2_shared).consensus(xs1_shared.length)

      val consensusErr =
        xs1_before.ee + joinShared.ee + xs2_after.ee

      def precise(d: Double): BigDecimal =
        BigDecimal valueOf d

      val _consensusScore =
        if(baseErr == 0 && consensusErr == 0)
          precise(1) // TODO is this reasonable?
        else
        (precise(baseErr) - 2*precise(consensusErr)) / (precise(baseErr) + 2*precise(consensusErr))

      if(_consensusScore > consensusScore) {
        consensusScore  = _consensusScore.toDouble
        consensus       = xs1_before ++ joinShared ++ xs2_after
        bov             = ovs(i)
      }

      i = i + 1
    }

    (consensus, consensusScore)
  }
}

```




[test/scala/BestOverlap.scala]: ../../test/scala/BestOverlap.scala.md
[test/scala/Intervals.scala]: ../../test/scala/Intervals.scala.md
[test/scala/Joiner.scala]: ../../test/scala/Joiner.scala.md
[main/scala/DNADistributions.scala]: DNADistributions.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/intervals.scala]: intervals.scala.md
[main/scala/io.scala]: io.scala.md
[main/scala/bestOverlap.scala]: bestOverlap.scala.md