package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions


trait Applicative[F[?]] extends Functor[F] { self =>
  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[?]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] =
    new Applicative[[X] =>> (F[X], G[X])] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }

  // Here we simply use `map2` to lift `apply` and `unit` themselves from one
  // Applicative into the other.
  // If `self` and `G` both satisfy the laws, then so does the composite.
  // The full proof can be found at
  // https://github.com/runarorama/sannanir/blob/master/Applicative.v
  given compose[G[?]] as Applicative[[X] =>> F[G[X]]] given (G: Applicative[G]) {
    def unit[A](a: => A) = self.unit(G.unit(a))
    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
      self.map2(fga, fgb)(G.map2(_, _)(f))
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

enum Validation[+E, +A] {
  case Failure(head: E, tail: Vector[E]) extends Validation[E, Nothing]
  case Success(a: A) extends Validation[Nothing, A]
}
import Validation._

object Applicative {
  given as Applicative[LazyList] {

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a)
    // Combine elements pointwise
    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])(f: (A, B) => C): LazyList[C] =
      a zip b map f.tupled
  }

  given [E] as Applicative[[X] =>> Validation[E, X]] {
    def unit[A](a: => A) = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
      (fa, fb) match
        case (Success(a),      Success(b))      => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e@Failure(_, _), _)               => e
        case (_,               e@Failure(_, _)) => e
  }

  type Const[A, B] = A

  given MonoidApplicative[M] as Applicative[[X] =>> Const[M, X] ] given (M: Monoid[M]) {
    def unit[A](a: => A): M = M.zero
    override def apply[A, B](m1: M)(m2: M): M = m1 op m2
  }
}

trait Monad[F[?]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(map(ma))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {

  // Notice that in the case of a `Left`, flatMap does nothing.
  given eitherMonad[E] as Monad[[X] =>> Either[E, X]] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match
        case Right(a) => f(a)
        case Left(b)  => Left(b)
    }

  given stateMonad[S] as Monad[[X] =>> State[S, X]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Monad composition
  given composeM[G[?], H[?]] as Monad[[X] =>> G[H[X]]] given (G: Monad[G], H: Monad[H], T: Traverse[H]) {
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(mna)(na => G.map(T.traverse(na)(f) given G)(H.join))
  }
}

trait Traverse[F[?]] extends Functor[F] with Foldable[F] { self =>
  def traverse[M[?]: Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))
  def sequence[M[?]:Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  given idMonad as Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B) given (mb: Monoid[B]): B =
    traverse[[X] =>> Const[B, X], A, Nothing](as)(f)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[[X] =>> State[S, X], A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)(a => (for
      i <- get[Int]
      _ <- set(i + 1)
    yield (a, i))).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)(a => (for
      as <- get[List[A]] // Get the current state, the accumulated list.
      _  <- set(a :: as) // Add the current element and set the new list as the new state.
    yield ())).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(a => (for
      s1      <- get[S]
      (b, s2) =  f(a, s1)
      _       <- set(s2)
    yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil)     => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil)     => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil)     => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  def fuse[M[?], N[?], A, B](fa: F[A])(f: A => M[B], g: A => N[B])
                         given (M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[[X] =>> (M[X], N[X]), A, B](fa)(a => (f(a), g(a)))(M product N)

  def compose[G[?]](G: Traverse[G]): Traverse[[X] =>> F[G[X]]] =
    new Traverse[[X] =>> F[G[X]]] {
      override def traverse[M[?]: Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)(G.traverse(_)(f))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  given listTraverse as Traverse[List] {
    override def traverse[M[?], A, B](as: List[A])(f: A => M[B]) given (M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  given optionTraverse as Traverse[Option] {
    override def traverse[M[?], A, B](oa: Option[A])(f: A => M[B]) given (M: Applicative[M]): M[Option[B]] =
      oa match
        case Some(a) => M.map(f(a))(Some(_))
        case None    => M.unit(None)
  }

  given treeTraverse as Traverse[Tree] {
    override def traverse[M[?], A, B](ta: Tree[A])(f: A => M[B]) given (M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  // An example of a Foldable that is not a functor
  case class Iteration[A](a: A, f: A => A, n: Int) {
    def foldMap[B](g: A => B)(M: Monoid[B]): B =
      def iterate(n: Int, b: B, c: A): B =
        if n <= 0 then b else iterate(n - 1, g(c), f(a))
      iterate(n, M.zero, a)
  }
}


