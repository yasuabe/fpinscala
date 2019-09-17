package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import given fpinscala.parallelism.Nonblocking.Par.ParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds
import language.implicitConversions

trait Monoid[A] {
  def (a1: A) op(a2: A): A
  def zero: A
}

object Monoid {

  given as Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  given [A] as Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  given intAddition as Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }

  given intMultiplication as Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val zero = 1
  }

  given booleanOr as Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y
    val zero = false
  }

  given booleanAnd as Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }

  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  given optionMonoid[A] as Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  given dual[A] as Monoid[A] given (m: Monoid[A]) {
    def (x: A) op (y: A): A = m.op(y)(x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual given firstOptionMonoid

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  given endoMonoid[A] as Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    given as Monoid[A] = m
    // Associativity
    forAll(for
      x <- gen
      y <- gen
      z <- gen
    yield (x, y, z))(p =>
      (p._1 op (p._2 op p._3)) == ((p._1 op p._2) op p._3)) &&
    // Identity
    forAll(gen)(a => (a op m.zero) == a && (m.zero op a) == a)

  def concatenate[A](as: List[A]) given (m: Monoid[A]): A =
    as.foldLeft(m.zero)(_ op _)

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](as: List[A])(f: A => B) given (m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => b op f(a))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    (foldMap(as)(f.curried) given endoMonoid[B])(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    (foldMap[A, B => B](as)(a => b => f(b, a)) given (dual given endoMonoid[B]))(z)

  def foldMapV[A, B](as: IndexedSeq[A])(f: A => B) given (m: Monoid[B]): B =
    if as.length == 0 then
      m.zero
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      (foldMapV(l)(f) op foldMapV(r)(f))

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean =
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    given as Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x

      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    (foldMapV(ints)(i => Option((i, i, true)))).map(_._3).getOrElse(true)

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  given par[A] as Monoid[Par[A]] given (m: Monoid[A]) {
    def zero = Par.unit(m.zero)
    def (a: Par[A]) op (b: Par[A]) = a.map2(b)(_ op _)
  }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A,B](v: IndexedSeq[A])(f: A => B) given (m: Monoid[B]): Par[B] =
    Par.parMap(v)(f).flatMap(foldMapV(_)(Par.lazyUnit))

  enum WC {
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)
  }
  import WC._

  given wcMonoid as Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }

  def count(s: String): Int =
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace then Part("", 0, "") else Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq)(wc) given wcMonoid match
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)

  given productMonoid[A,B] as Monoid[(A, B)] given (A: Monoid[A], B: Monoid[B]) {
    def (x: (A, B)) op (y: (A, B)) =
      (A.op(x._1)(y._1), B.op(x._2)(y._2))
    val zero = (A.zero, B.zero)
  }

  given functionMonoid[A,B] as Monoid[A => B] given (B: Monoid[B]) {
    def (f: A => B) op (g: A => B) = (a: A) => f(a) op g(a)
    val zero: A => B = a => B.zero
  }

  given mapMergeMonoid[K,V] as Monoid[Map[K, V]] given (V: Monoid[V]) {
    def zero = Map[K,V]()
    def (a: Map[K, V]) op (b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, a.getOrElse(k, V.zero) op b.getOrElse(k, V.zero))
      }
  }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as)(a => Map(a -> 1)) given (mapMergeMonoid[A, Int] given intAddition)

}

trait Foldable[F[?]] {
  import Monoid._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    (foldMap(as)(f.curried) given endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    (foldMap(as)(a => (b: B) => f(b, a)) given (dual given endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B) given (mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => f(a) op b)

  def concatenate[A](as: F[A]) given (m: Monoid[A]): A =
    foldLeft(as)(m.zero)(_ op _)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B) given (mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => b op f(a))
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B) given (mb: Monoid[B]): B =
    foldMapV(as)(f) given mb
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])
}
import Tree._

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B) given (mb: Monoid[B]): B = as match
    case Leaf(a) => f(a)
    case Branch(l, r) =>foldMap(l)(f) op foldMap(r)(f)

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
}

// Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `op`. That kind of object (a monoid without a `zero`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B) given (mb: Monoid[B]): B =
    as match
      case None => mb.zero
      case Some(a) => f(a)

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match
    case None => z
    case Some(a) => f(z, a)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match
    case None => z
    case Some(a) => f(a, z)
}

