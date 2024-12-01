package fpinscala.exercises.parallelism

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]
object Par:
  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] =
    es =>
      UnitFuture(
        a
      ) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A])
    def map2[B, C](
        pb: Par[B]
    )(
        f: (A, B) => C
    ): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
      es =>
        val af = pa(es)
        val bf = pb(es)
        UnitFuture(
          f(af.get, bf.get)
        ) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.

  def fork[A](
      a: => Par[A]
  ): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) =
    parList.map(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, pas) => pa.map2(pas)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork:
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    sequence(
      as.map(a => if (f(a)) then unit(List(a)) else unit(List.empty))
    ).map(_.flatten)
