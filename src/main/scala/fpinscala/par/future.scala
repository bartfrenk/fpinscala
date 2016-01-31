package fpinscala.par

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

/** Implementation of 'Par' that makes use of [[java.util.concurrent.Future]]. */
object FutureImpl {

  // Forced to be invariant in type parameter due to invariance of
  // [[java.util.concurrent.Future]].
  type Par[A] = ExecutorService => Future[A]

  def run[A](a: Par[A])(implicit s: ExecutorService): A = a(s).get

  /** Wraps an evaluated computation with a [[java.util.concurrent.Future]]. */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  object Future {
    /** Combines two futures into one.
      * Implementation does not take timeouts into account when evaluating f. */
    def map2[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C): Future[C] =
      new Future[C] {
        def isDone = a.isDone && b.isDone
        def get(timeout: Long, units: TimeUnit) = {
          val (r, t) = Time.duration(a.get(timeout, units), units)
          val s = b.get(timeout - t, units)
          f(r, s)
        }
        def get = f(a.get, b.get)
        def isCancelled = a.isCancelled || b.isCancelled
        def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      }
  }

  object Monad extends ParMonad[Par] {
    def get[A](a: Par[A]): A = undefined
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        Future.map2(a(es), b(es))(f)
      }
    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        // blocks on new thread, and causes deadlock when 'es'
        // has a fixed number of threads.
        def call = a(es).get
      })
    }
    def unit[A](a: A): Par[A] = s => UnitFuture(a)
    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def equal[A](e: ExecutorService)(p: Par[A], q: Par[A]): Boolean =
      p(e).get == q(e).get
  }
}

