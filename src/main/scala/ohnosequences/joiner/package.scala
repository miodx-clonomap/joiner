package ohnosequences

import spire.implicits._

/**
 * This package exposes functions to work with sequences and joint probability
 * distributions.
 */
package object joiner {

  type DNASeq = Array[DNADistributions.DNAD]

  /**
   * Convert a Phred quality score to an error probability
   */
  def scoreToErrorProb(n: Int): Double =
    Math.max(
      consensus.minErr,
      (10:Double) fpow ( - ((n:Double) / 10) )
    )

  /**
   * Convert an error probability to a Phred quality score
   */
  def errorProbToScore(err: Double): Int =
    Math.min(
      consensus.maxQScore,
      Math.round((-10:Double) * err.log(10)).toInt
    )
}
