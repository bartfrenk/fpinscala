package fpinscala.par

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future}
import java.util.concurrent.atomic.AtomicReference

/** Implementation of 'Par' that does not need to block to get the result. */
object NonBlockingImpl {

  type Par[+A] = ExecutorService => Future[A]

  /** Trait that supports registering a callback. */
  sealed trait Future[+A] {
    def apply(k: A => Unit): Unit
  }

  /** Blocks until computation finishes and returns result. */
  def run[A](p: Par[A])(implicit es: ExecutorService): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  object Monad extends ParMonad[Par] {
    def delay[A](a: => Par[A]): Par[A] = es => a(es)
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        /** Extracts [[Future]] from 'a' and applies it to 'cb' asynchronously. */
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {

          val initial: (Option[A], Option[B]) = (None, None)
          // To avoid race conditions, the atomic reference needs to include
          // the values of type A and B simultaneously. It is not sufficient to
          // have an atomic reference of type Option[A], and one of Option[B].
          val ref = new AtomicReference(initial)

          a(es)(a => {
            // need to compare actual reference 'initial', not just its value
            val lacksOther = ref.compareAndSet(initial, (Some(a), None))
            if (!lacksOther) {
              ref.get match {
                case (_, Some(b)) => cb(f(a, b))
                case _ => throw new Exception(s"Callback for $a entered twice. Not supposed to happen.")
              }
            }
          })

          b(es)(b => {
            // need to compare exact reference 'initial', not just its value
            val lacksOther = ref.compareAndSet(initial, (None, Some(b)))
            if (!lacksOther) {
              ref.get match {
                case (Some(a), _) => cb(f(a, b))
                case _ => throw new Exception(s"Callback for $b entered twice. Not supposed to happen.")
              }
            }
          })
        } // apply
      } // Future
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        /** Applies 'cb' to 'a' synchronously (i.e., on the calling thread). */
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** Evaluates computation asynchronously. */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })
  }

}
