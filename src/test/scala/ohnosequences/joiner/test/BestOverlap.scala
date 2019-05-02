package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.joiner._, DNADistributions._, intervals._, bestOverlap._

class BestOverlap extends FunSuite {

  val A = DNAD(A = 0.99, T = 0.005, C = 0.0025, G = 0.0025)
  val T = DNAD(T = 0.99, A = 0.005, C = 0.0025, G = 0.0025)
  val G = DNAD(G = 0.99, T = 0.005, C = 0.0025, A = 0.0025)

  def As(no: Int) =
    Array.fill(no)(A)
  def Ts(no: Int) =
    Array.fill(no)(T)
  def Gs(no: Int) =
    Array.fill(no)(G)

  test("") {

    val more =
      7
    val less =
      0

    val seq1 =
      Array(A,T)

    val seq2 =
      Array(T,A)

    val AT: Array[DNASeq] =
      Array.fill(more)(seq1) ++ Array.fill(less)(seq2)

    val AT_again: Array[DNASeq] =
      Array.fill(less)(seq2) ++ Array.fill(more)(seq1)

    val AT_consensus =
      AT consensus 2
    val AT_consensus_again =
      AT_again consensus 2

    println(s"AT         : ${AT_consensus.show}")
    println(s"AT (again) : ${AT_consensus_again.show}")
  }

  test("find best overlap") {

    val d1 = Array(T,T,T,T,G,G,G,G)
    val d2 =         Array(G,G,G,G,A,A,A,A,A,A)

    val (d,s) = bestOverlap.between(d1,d2)

    assert( d.map(_.mostLikely).toArray === "TTTTGGGGAAAAAA".toArray )
  }

  test("find best overlap from fastq") {
    import ohnosequences.fastarious._, fastq._, DNAQ._

    val qual = 200

    val s1 =
      sequenceQualityToDNASeq(
        SequenceQuality(
          Sequence("GCCTTTTCTCAGGGGAGAGGCCATCACTTGAAGATGCTGAGTCTTCTGCTCCTTCTCCTGGGACTAGGCTCTGTGTTCAGTGCTGTCATCTCTCAAAAGCCAAGCAGGGATATCTGTCAACGTGGAACCTCCCTGACGATCCAGTGTCAAGTCGATAGCCAAGTCACCATGATGTTCTGGTACCGTCAGCAACCTGGACAGAGCCTGACACTGATCGCAACTGCAAATCAGGGCTCTGAGGCCACATATGAGAGTGGATTTGTCATTGACAAGTTTCC"),
          Quality(Seq.fill(278)(qual))
        )
      )

    val s2 =
      sequenceQualityToDNASeq(
        SequenceQuality(
          Sequence("TGGGAACACCTTGTTCAGGTCCTCTACAACGGTTAACCTGGTCCCCGAACCGAAGGTGTAGCCATAGTTAGCTCCTGTTCCAACGCTGCAGAGATATATGCTGCTGTCTTCAGGGCTCATGTTGCTCACAGTCAGAGTTGAGAATGTTAGGTTTGGGCGGCTGATGGGAAACTTGTCAATGACAAATCCACTCTCATATGTGGCCTCAGAGCCCTGATTTGCAGTTGCGATCAGTGTCAGGCTCTGTCCAGGTTGCTGACGGTACCAGAACATCATGGTGACTTGGCTATCGACTTGACACTGGATCGTCAGGGAGGTTCCACGTTGACA"),
          Quality(Seq.fill(330)(qual))
        ).asDNAQ.complement.reverse
      )

    assert(bestOverlap.between(s1,s2)._2 == 1)
  }
}
