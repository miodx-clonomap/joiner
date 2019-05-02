package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.fastarious._, fastq._
import java.nio.file._
import scala.collection.JavaConverters._
import java.io._

import ohnosequences.joiner._, DNADistributions._
import io._

class JoinerTest extends FunSuite {

  val in = new File("../fastarious/in1.fastq")

  def lines(jFile: File): Iterator[String] =
    Files.lines(jFile.toPath).iterator.asScala

  def pSymbols =
    lines(in).parseFastqPhred33DropErrors
      .map(_.sequence.pSymbols)

  def dnads =
    pSymbols map pSymbolsToDNASeq

  test("DNA distributions") {

    val d1 = DNAD(A = 0.80, T = 0.1, C = 0.05, G = 0.05)

    assert(d1.mostLikely == 'A')

    // assert(d1.errorProb == 0.20)
  }

  ignore("DNADs to SequenceQuality") {

    dnads foreach { ds => println(ds.toSequenceQuality) }
  }

  ignore("DNADs from fastq file -- ee") {

    dnads foreach { ds => val z = ds.head.mostLikely }
  }

  ignore("DNADs from fastq file -- joinAll") {

    dnads.grouped(1000) foreach { dsds =>
      // 1000 reads, try to build consensus for all of them
      // first index: read
      // second index: pos
      // val cluster = Array.fill[Array[DNAD]](1000)(ds)

      val c = dsds.toArray.consensus(250)

      println(c.show)
    }
  }

  test("consensus order") {

    val less = 100
    val more = 300

    val AT = Seq(QSymbol('A', 10), QSymbol('T', 10))
    val TT = Seq(QSymbol('T', 10), QSymbol('T', 10))

    val ds1 = Array.fill(less)(AT) ++ Array.fill(more)(TT)
    val ds2 = Array.fill(more)(TT) ++ Array.fill(less)(AT)

    println(s"These two sequences should be equal: ")
    println(consensus.of(ds1)(2).show)
    println(consensus.of(ds2)(2).show)
  }

  test("simple consensus computation") {

    val A1 = DNAD(A = 0.80, T = 0.1, C = 0.05, G = 0.05)
    val A2 = DNAD(A = 0.89, T = 0.01, C = 0.05, G = 0.05)
    val T1 = DNAD(A = 0.01, T = 0.89, C = 0.05, G = 0.05)

    val d1 = Array(A1,A2,T1,A2)
    val d2 = Array(A2,A2,T1,T1)

    val alignment: Array[DNASeq] = Array(d1,d2,d1,d1) ++ Array.fill(5)(d2)
  }

  ignore("Create pSymbols from fastq file") {

    pSymbols foreach { seq => val z = seq.length }
  }
}
