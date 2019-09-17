package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[?]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def product[G[?]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] = ???

  def compose[G[?]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[?]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[[X] =>> Either[E, X]] = ???

  given stateMonad[S] as Monad[[X] =>> State[S, X]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[?],N[?]](F: Monad[F], N: Monad[N]) given (T: Traverse[N]):
    Monad[[X] =>> F[N[X]]] = ???
}

enum Validation[+E, +A] {
  case Failure(head: E, tail: Vector[E]) extends Validation[E, Nothing]
  case Success(a: A) extends Validation[Nothing, A]
}
import Validation._

object Applicative {

  given streamApplicative as Applicative[LazyList] {

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: LazyList[A], b: LazyList[B])( // Combine elements pointwise
                    f: (A,B) => C): LazyList[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[[X] =>> Validation[E, X]] = ???

  type Const[A, B] = A

  given monoidApplicative[M] as Applicative[[X] =>> Const[M, X]] given (M: Monoid[M]) {
    def unit[A](a: => A): M = M.zero
    override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
  }
}

trait Traverse[F[?]] extends Functor[F] with Foldable[F] {
  def traverse[G[?]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[?]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B) given (mb: Monoid[B]): B =
    traverse[[X] =>> Const[B, X], A, Nothing](as)(f)

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[[X] =>> State[S, X], A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(a => (for
      s1      <- get[S]
      (b, s2) =  f(a, s1)
      _       <- set(s2)
    yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[?],H[?],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         given (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[?]] given (G: Traverse[G]): Traverse[[X] =>> F[G[X]]] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
