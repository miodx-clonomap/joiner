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

      val _consensusScore = (baseErr - 2*consensusErr) / (baseErr + 2*consensusErr)

      if(_consensusScore > consensusScore) {
        consensusScore  = _consensusScore
        consensus       = xs1_before ++ joinShared ++ xs2_after
        bov             = ovs(i)
      }
      
      i = i + 1
    }

    (consensus, consensusScore)
  }
}
