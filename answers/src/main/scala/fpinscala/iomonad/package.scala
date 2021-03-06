package fpinscala.iomonad

import language.higherKinds

import java.util.concurrent.ExecutorService
import fpinscala.parallelism.Nonblocking._
import IO3.Free.{Return => FreeReturn, _}

type IO[A] = IO3.IO[A]
def IO[A](a: => A): IO[A] = IO3.IO[A](a)

given ioMonad as Monad[[X] =>> IO3.Free[Par, X]] = IO3.freeMonad[Par]

def now[A](a: A): IO[A] = Return(a)

def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())) flatMap (_ => a)

def forkUnit[A](a: => A): IO[A] = fork(now(a))

def delay[A](a: => A): IO[A] = now(()) flatMap (_ => now(a))

def par[A](a: Par[A]): IO[A] = Suspend(a)

def async[A](cb: ((A => Unit) => Unit)): IO[A] =
  fork(par(Par.async(cb)))

type Free[F[?], A] = IO3.Free[F, A]

def Return[A](a: A): IO[A] = FreeReturn[Par, A](a)

// To run an `IO`, we need an executor service.
// The name we have chosen for this method, `unsafePerformIO`,
// reflects that is is unsafe, i.e. that it has side effects,
// and that it _performs_ the actual I/O.
def unsafePerformIO[A](io: IO[A]) given (E: ExecutorService): A =
  Par.run { IO3.run(io) given IO3.parMonad }
