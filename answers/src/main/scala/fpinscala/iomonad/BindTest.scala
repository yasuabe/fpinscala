package fpinscala.iomonad

import language.higherKinds
import language.postfixOps
import language.implicitConversions

object BindTest extends App {

  def timeit(n: Int)(task: => Unit): Unit =
    val start = System.currentTimeMillis
    (0 to n).foreach { _ => task }
    val stop = System.currentTimeMillis
    println(s"${(stop - start) / 1000.0} seconds")

  val N = 100000

  def go[F[?]](unit: F[Unit])(f: F[Int] => Int) given (F: Monad[F]): Unit =
    import given F.toMonadic
    f((0 to N).map(i => F.map(unit)(_ => i)).foldLeft(F.unit(0))(
      (f1, f2) => for
        acc <- f1
        i   <- f2
      yield (acc + i) // if (i == N) println("result: " + (acc+i))
    ))

  import fpinscala.parallelism.Nonblocking._
  import java.util.concurrent.ExecutorService

  object ParMonad extends Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(pa)(f) }
  }

  given Pool as ExecutorService = java.util.concurrent.Executors.newFixedThreadPool(4)

  timeit(10) { go(Throw.unit(())) ( _ run ) given Throw }
  timeit(10) { go(IO2b.TailRec.unit(())) ( IO2b.run ) given IO2b.TailRec }
  timeit(10) { go(IO2c.Async.unit(()))(r => Par.run { IO2c.run(r) }) given IO2c.Async }
  timeit(10) { go[IO](ioMonad.unit(()))(r => unsafePerformIO(r)) given ioMonad }
  timeit(10) { go(Task.now(()))(r => r.run) given Task }
  timeit(10) { go(Task.forkUnit(()))(r => r.run) given Task }
  timeit(10) { go(ParMonad.unit(())) { p => Par.run(p) } given ParMonad }

  // Par.run(pool)(ParMonad.forever { ParMonad.unit { println("woot") }})
  Pool.shutdown()
}
