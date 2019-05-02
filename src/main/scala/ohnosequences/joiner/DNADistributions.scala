package ohnosequences.joiner

import ohnosequences.fastarious._

/**
 * Namespace wrapping functions related with DNADistributions
 */
case object DNADistributions {

  /**
   * A DNADistribution, where each nucleotide has one probability assigned, and
   * p(A) + p(T) + p(C) + p(G) = 1.0
   *
   * @param A is the probability of the A nucleotide
   * @param T is the probability of the T nucleotide
   * @param C is the probability of the C nucleotide
   * @param G is the probability of the G nucleotide
   */
  case class DNAD(
    val A: Prob,
    val T: Prob,
    val C: Prob,
    val G: Prob
  )
  {
    /**
     * Retrieve the probability of a nucleotide given its character. This
     * crashes if the specified char is different thatn A, T, C, G or N
     */
    def apply(c: Char): Prob =
      c match {
        case 'A' => A
        case 'T' => T
        case 'C' => C
        case 'G' => G
        case 'N' => 1
      }

    /** The nucleotide characters sorted by decreasing probability */
    val sortedBases: Array[Char] =
      Array[Char]('A','T','C','G').sortBy(1 - this(_))

    /** Return the most likely nucleotide */
    def mostLikely: Char =
      sortedBases.head

    /**
     * Return the error probability, which is the complementary of the most
     * likely nucleotide
     */
    def errorProb: Prob = 1 - this(mostLikely)
  }

  /**
   * Build a uniform DNAD, with the probability of all nucleotides equal to 0.25
   */
  val uniform: DNAD =
    DNAD(A = 0.25, T = 0.25, C = 0.25, G = 0.25)


  // conversions
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Given a fastarious.PSymbol, build the corresponding DNAD by assigning the
   * known probability to the most likely nucleotide and the rest of the
   * probability equally* shared among the other three.
   */
  val pSymbolToDNAD: PSymbol => DNAD =
    {
      case PSymbol('A', err) =>
        DNAD(A = 1 - err, T = err/3, C = err/3, G = err/3)
      case PSymbol('T', err) =>
        DNAD(T = 1 - err, A = err/3, C = err/3, G = err/3)
      case PSymbol('C', err) =>
        DNAD(C = 1 - err, T = err/3, A = err/3, G = err/3)
      case PSymbol('G', err) =>
        DNAD(G = 1 - err, T = err/3, C = err/3, A = err/3)
      case _ => uniform
    }

  /** Convert a [[DNAD]] to a fastarious.PSymbol */
  val DNADtoPSymbol: DNAD => PSymbol =
    d => PSymbol(d.mostLikely, d.errorProb)

  /** Convert a fastarious.SequenceQuality to [[DNASeq]] */
  val sequenceQualityToDNASeq: SequenceQuality => DNASeq =
    { x: SequenceQuality => x.pSymbols } andThen pSymbolsToDNASeq

  /** Convert a sequence of fastarious.PSymbol to [[DNASeq]] */
  lazy val pSymbolsToDNASeq: Seq[PSymbol] => DNASeq =
    qss => {

      val ds: Array[DNAD] = Array.fill(qss.length)(null)

      var i = 0
      while(i < ds.length) {
        ds.update(i, pSymbolToDNAD(qss(i)))
        i = i + 1
      }

      ds
    }

  /**
   * Compute the Δ probability of the joint distribution of two DNADs, which is
   * defined as the sum of the product of each probability
   */
  def deltaProb(d1: DNAD, d2: DNAD): Prob =
    (d1.A * d2.A) + (d1.T * d2.T) + (d1.C * d2.C) + (d1.G * d2.G)

  /**
   * Compute the joint distribution of two DNADs
   */
  def join(d1: DNAD, d2: DNAD): DNAD =
    DNAD(
      A = (d1.A * d2.A) / deltaProb(d1,d2),
      T = (d1.T * d2.T) / deltaProb(d1,d2),
      C = (d1.C * d2.C) / deltaProb(d1,d2),
      G = (d1.G * d2.G) / deltaProb(d1,d2)
    )

  /**
   * When imported, this adds the functions defined inside to Array[DNASeq]
   */
  implicit class ConsensusOps(val dsds: Array[DNASeq]) extends AnyVal {

    /**
     * Compute the join of distributions at each column
     */
    def consensus(len: Int): Array[DNAD] = {

      def read(i: Int) = dsds(i)

      val cons = Array.fill[DNAD](len)(uniform)
      var i   = 0 // read
      var pos = 0 // position

      // iter reads
      while(pos < len) {
        while(i < dsds.length) {
          var pos_res = uniform
          while(i < dsds.length) {
            pos_res = join( pos_res, read(i)(pos) )
            i = i + 1
          }
          cons.update(pos, pos_res)
        }
        i = 0
        pos = pos + 1
      }
      cons
    }
  }

  /**
   * When imported, this adds the functions defined inside to a [[DNASeq]]
   */
  implicit class DNADOps(val ds: DNASeq) extends AnyVal {

    /** Builds a string describing the DNASeq */
    def show: String =
      ds.foldLeft(""){ (acc, d) =>
        acc ++ s"${d.mostLikely}:${d.errorProb.toString}|"
      }

    /** Compute the joint distribution of all elements in DNASeq */
    def joinAll: DNAD = {

      var i   = 0
      var d   = uniform
      var res = d

      while(i < ds.length) { res = join(d,res); i = i + 1 }

      res
    }

    /**
     * Build a DNASeq equal to this DNASeq except for the positions where the
     * error probability is greater than a threshold, where a uniform DNAD is
     * written.
     */
    def uniformOver(threshold: ErrorP): DNASeq = {

      val res = Array.fill[DNAD](ds.length)(uniform)
      var i = 0
      while(i < ds.length) {
        if(ds(i).errorProb <= threshold) { res.update(i, ds(i)) }
        i = i+1
      }

      res
    }

    /** Number of expected errors of this [[DNASeq]] */
    def ee: Double = {
      var sum = 0D
      var i = 0
      while(i < ds.length) {
        sum = ds(i).errorProb + sum
        i = i + 1
      }
      sum
    }

    /** Convert this [[DNASeq]] to a fastarious.SequenceQuality */
    def toSequenceQuality: SequenceQuality = {

      val seq =
        Sequence( String.valueOf(ds.map(_.mostLikely)) )
      val scores =
        Quality( ds.map(d => ((-10) * Math.log10(d.errorProb)).intValue).toSeq )

      SequenceQuality(seq, scores)
    }
  }
}
