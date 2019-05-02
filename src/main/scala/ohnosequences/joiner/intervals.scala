package ohnosequences.joiner

/**
 * Namespace wrapping functions to work with intervals
 * @type {[type]}
 */
case object intervals {

  /** A Natural number */
  type Nat = Int

  /**
   * An integer is a Natural number if and only if it is greater or equal than
   * zero
   */
  def isNat(n: Int): Boolean =
    n >= 0

  /**
   * Build an [[Interval]] with a specified start and end, constrained to the
   * Natural numbers [0, ∞[
   *
   * @param s is the start of the interval; if it is negative, 0 is taken as
   * the interval start
   * @param e is the end of the interval; it it is negative, 0 is taken as the
   * interval end
   *
   * @note If s >= e, the [[Empty]] [[Interval]] is returned
   *
   * @return the interval [s,e[ ∩ [0, ∞[
   */
  def interval(s: Int, e: Int): Interval =
    if(s >= e)
      Empty
    else
      if(isNat(s) && isNat(e))
        LCRO(s, e)
      else
        interval(Math.max(0,s), Math.max(0,e))

  /**
   * An Interval specifying the start and end as Natural numbers
   */
  sealed trait Interval {
    /** The start of the Interval */
    def start : Nat

    /** The end of the interval */
    def end   : Nat

    /** The Interval [0, s[ */
    def before: Interval =
      interval(0, start)

    /** The interval [e, ∞[ */
    def after: Interval =
      interval(end, Int.MaxValue)

    def isEmpty: Boolean = end <= start
    def length = end - start
  }
  /**
   * A Left Closed Right Open interval; i.e., [start, end[
   */
  case class LCRO(start: Nat, end: Nat) extends Interval {

    override def toString =
      s"[${start},${end}["
  }

  /**
   * The Empty interval, defined as [0, 0[
   */
  case object Empty extends Interval {
    def start = 0
    def end = 0

    override def toString = "∅"
  }

  /**
   * The overlap of two intervals, defining the prefix, the overlap part and
   * the suffix.
   *
   * Let [x0, x1] and [x2, x3] two intervals layout as follows:
   * -----[x0-----[x2------x1]--------x3]--------
   * Then, the prefix will be [x0, x2], the overlap will be [x2, x1] and the
   * suffix will be [x2, x3]
   */
  case class OverlapHC(
    prefix  : Interval,
    overlap : Interval,
    suffix  : Interval
  ) {

    /** The length of the prefix and overlap intervals */
    def firstLength =
      prefix.length + overlap.length

    /** The length of the overlap and suffix intervals */
    def secondLength =
      suffix.length + overlap.length
  }

  /**
   * A relative ovelap
   */
  case class RelativeOverlap(
    firstPrefix   : Interval,
    firstOverlap  : Interval,
    secondOverlap : Interval,
    secondSuffix  : Interval
  )

  /** Check whether two intervals are disjoint */
  def disjoint(i1: Interval, i2: Interval): Boolean =
    i1.end <= i2.start

  /** Check whether two intervals overlap */
  def overlapping(i1: Interval, i2: Interval): Boolean =
    i1.start < i2.end && i2.start < i1.end

  /** Build an interval [0, l[, where l is the length of an array */
  def whole[X](seq: Array[X]): Interval =
    interval(0, seq.length)

  /** Translate an interval `add` positions */
  def translate(add: Int)(i: Interval): Interval =
    interval(i.start + add, i.end + add)

  /** Build the interval i1 ∩ i2 */
  def intersection(i1: Interval, i2: Interval): Interval =
    interval(Math.max(i1.start, i2.start), Math.min(i1.end, i2.end))

  /** Build the [[OverlapHC]] of two intervals **/
  def overlap(first: Interval, second: Interval): OverlapHC = {

    val shared = intersection(first,second)

    OverlapHC(
      intersection(first, shared.before),
      shared,
      intersection(second, shared.after)
    )
  }

  /**
   * Move a sliding window of the specified length along the specified interval
   * and return, for each position, the intervals that result of intersecting
   * the window and the specified interval; namely
   * (before, both_s1, both_s2, after) where before is a subinterval of s1
   */
  def overlapsWithLength(length: Int)(s1: Interval)
  : Array[(Interval,Interval, Interval, Interval)] =
    Array.tabulate(s1.length)({ i =>
      val shared = interval(s1.start + i, s1.start + i + length)
      (
        intersection(s1, shared.before),
        intersection(s1, shared),
        translate(-i)(intersection(s1,shared)),
        translate(-i)(intersection(s1.after, shared))
      )
    })
}
