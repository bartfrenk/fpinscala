package fpinscala.par

import akka.actor._

object ParActor {
  val system = ActorSystem("Par")

  type Par[+A] = ActorRefFactory => Future[A]

  object Evaluator {
    case class Expression[A](a: Unit => Par[A], cb: A => Unit)
  }

  /** Evaluates parallel computations. */
  class Evaluator extends Actor {
    import Evaluator._
    def receive = {
      case Expression(a, cb) => a(())(context)(cb)
    }
  }

  object Combiner {
    def props[A, B, C](cb: C => Unit, f: (A, B) => C): Props = Props(new Combiner(cb, f))
  }

  /** Actor that applies a function to the results of two parallel computations. */
  class Combiner[A, B, C](cb: C => Unit, f: (A, B) => C) extends Actor {
    var ar: Option[A] = None
    var br: Option[B] = None
    def receive = {
      case result: Either[A, B] => result match {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => cb(f(a, b))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => cb(f(a, b))
        }
      }
    }
  }

  sealed trait Future[+A] {
    /** Registers a callback function. */
    def apply(cb: A => Unit): Unit
  }

  /** Waits on the result of a parallel computation. */
  def run[A](a: Par[A]): A = Monad.get(a)

  object Monad extends ParMonad[Par] {
    def get[A](a: Par[A]): A = undefined
    def delay[A](a: => Par[A]) = fac => a(fac)
    def map2[A, B, C](p: Par[A], q: Par[B])(f: (A, B) => C): Par[C] = fac => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        val combiner = fac.actorOf(Combiner.props(cb, f))
        p(fac)(a => combiner ! Left(a))
        q(fac)(b => combiner ! Right(b))
      }
    }

    def unit[A](a: A): Par[A] = fac => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
    def fork[A](a: => Par[A]): Par[A] = fac => new Future[A] {
      def apply(cb: A => Unit): Unit =
        fac.actorOf(Props[Evaluator]) ! Evaluator.Expression(_ => a, cb)
    }

  }
}
