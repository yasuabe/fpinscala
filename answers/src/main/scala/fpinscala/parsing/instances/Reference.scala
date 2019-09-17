package fpinscala
package parsing

import ReferenceTypes._
import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = given ParseState => Result[A]

  /** `ParseState` wraps a `Location` and provides some extra
    * convenience functions. The sliceable parsers defined
    * in `Sliceable.scala` add an `isSliced` `Boolean` flag
    * to `ParseState`.
    */
  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  /* Likewise, we define a few helper functions on `Result`. */
  enum Result[+A] {
    def extract: Either[ParseError,A] = this match
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)

    /* Used by `attempt`. */
    def uncommit: Result[A] = this match
      case Failure(e,true) => Failure(e,false)
      case _ => this

    /* Used by `flatMap` */
    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this

    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e,c) => Failure(f(e),c)
      case _ => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a,m) => Success(a,n+m)
      case _ => this

    case Success(get: A, length: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[A]
  }

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i < s1.length && i < s2.length do
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1

    if s1.length-offset >= s2.length then -1
    else s1.length-offset
}

object Reference extends Parsers[Parser] {
  import ReferenceTypes.Result._

  def run[A](p: Parser[A])(s: String): Either[ParseError,A] =
    val s0 = ParseState(Location(s))
    (p given s0).extract

  // consume no characters and succeed with the given value
  def succeed[A](a: A): Parser[A] = Success(a, 0)

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] = p match
    case Failure(e, false) => p2
    case r                 => r // committed failure or success skips running `p2`

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = f match
    case Success(a, n)   => (g(a) given the[ParseState].advanceBy(n))
                              .addCommit(n != 0)
                              .advanceSuccess(n)
    case f@Failure(_, _) => f.asInstanceOf[Result[B]]

  def string(w: String): Parser[String] =
    val s = the[ParseState]
    val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
    if (i == -1) Success(w, w.length) // they matched
    else         Failure(s.loc.advanceBy(i).toError("'" + w + "'"), i != 0)

  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */
  def regex(r: Regex): Parser[String] =
    r.findPrefixOf(the[ParseState].input) match
      case Some(m) => Success(m, m.length)
      case None    => Failure(the[ParseState].loc.toError("regex " + r), false)

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    p.mapError(_.push(the[ParseState].loc,msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    p.mapError(_.label(msg))

  def fail[A](msg: String): Parser[A] =
    Failure(the[ParseState].loc.toError(msg), true)

  def attempt[A](p: Parser[A]): Parser[A] =
    p.uncommit

  def slice[A](p: Parser[A]): Parser[String] = p match
    case Success(_, n)   => Success(the[ParseState].slice(n), n)
    case f@Failure(a, b) => f.asInstanceOf[Result[String]]

  /* We provide an overridden version of `many` that accumulates
   * the list of results using a monolithic loop. This avoids
   * stack overflow errors for most grammars.
   */
  override def many[A](p: Parser[A]): Parser[List[A]] =
    var nConsumed = 0
    val buf = collection.mutable.ListBuffer[A]()
    def go(p: Parser[A], offset: Int): Result[List[A]] =
      (p given the[ParseState].advanceBy(offset)) match
        case Success(a, n)      => buf += a; go(p, offset+n)
        case f@Failure(e, true) => f.asInstanceOf[Result[List[A]]]
        case Failure(e, _)      => Success(buf.toList, offset)
    go(p, 0)
}

