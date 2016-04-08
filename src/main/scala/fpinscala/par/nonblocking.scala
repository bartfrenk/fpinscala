package fpinscala.par

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future}
import java.util.concurrent.atomic.AtomicReference
import scala.util.{Try, Success, Failure}

/** Implementation of 'Par' that does not need to block to get the result. */
object NonBlockingImpl {

  type Par[+A] = ExecutorService => Future[A]

  /** Trait that supports registering a callback. */
  sealed trait Future[+A] {
    def apply(k: Try[A] => Unit): Unit
  }

  /** Blocks until computation finishes and returns result. */
  def run[A](p: Par[A])(implicit es: ExecutorService): Try[A] = {
    val ref = new AtomicReference[Try[A]]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  object TryOps {
    /** Evaluates function at arguments, returns failure if one of the arguments is. */
    def map2[A, B, C](ta: Try[A], tb: Try[B])(f: (A, B) => C): Try[C] =
      ta flatMap (a => (tb map (b => f(a, b))))
    /** Construct a generic failure with specified error message. */
    def fail[A](msg: String): Try[A] = Failure(new Exception(msg))
  }

  object Monad extends ParMonad[Par] {
    override def flatMap[A, B](A: Par[A])(f: A => Par[B]): Par[B] = undefined
    def delay[A](a: => Par[A]): Par[A] = es => a(es)
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        /** Extracts [[Future]] from 'a' and applies it to 'cb' asynchronously. */
        def apply(cb: Try[A] => Unit): Unit =
          eval(es)(a(es), cb)
      }
    /** Applies function to the result of two possibly asynchronous computations.
      * Does not wait for the last computation if the first fails. */
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: Try[C] => Unit): Unit = {

          val initial: Option[Either[A, B]] = None
          // To avoid race conditions, the atomic reference needs to include
          // the values of type A and B simultaneously. It is not sufficient to
          // have an atomic reference of type Option[A], and one of type Option[B].
          val ref = new AtomicReference(initial)

          /** Possibly returns value to be passed to the final callback. */
          def update(received: Either[A, B]): Option[Try[C]] = {
            // need to compare actual reference 'initial', not just its value
            val hasOther = !ref.compareAndSet(initial, Some(received))
            if (hasOther) {
              (received, ref.get) match {
                case (Left(a), Some(Right(b))) => Some(Success(f(a, b)))
                case (Right(b), Some(Left(a))) => Some(Success(f(a, b)))
                case _ => Some(TryOps.fail("Callback entered twice. Not supposed to happen."))
              }
            } else None
          }

          pa(es) {
            case Failure(exception) => cb(Failure[C](exception))
            case Success(a) => update(Left(a)) match {
              case Some(tc) => cb(tc)
              case _ => ()
            }
          }

          pb(es) {
            case Failure(exception) => cb(Failure[C](exception))
            case Success(b) => update(Right(b)) match {
              case Some(tc) => cb(tc)
              case _ => ()
            }
          }
        } // apply
      } // Future

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        /** Applies 'cb' to 'a' synchronously (i.e., on the calling thread). */
        def apply(cb: Try[A] => Unit): Unit =
          cb(Success(a))
      }

    /** Evaluates computation asynchronously. */
    def eval[A](es: ExecutorService)(f: => Future[A], cb: Try[A] => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = {
          // force evaluation of 'f' within a try-catch block, while still
          // having access to the evaluated version outside of that block
          lazy val future = f
          try {
            future
          } catch {
            case e: Exception => cb(Failure(e))
          }
          future(cb)
        }
    })
  }

}
