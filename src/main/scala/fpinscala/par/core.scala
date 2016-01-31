package fpinscala.par

import scala.language.higherKinds
import scala.language.implicitConversions

/** Adds monadic combinators to a specific 'Par' implementation. */
trait ParMonad[Par[_]] {self =>
  def undefined = throw new Exception("undefined function")

  /** Allows for infix notation of certain 'Par' combinators. */
  implicit def operators[A](p: Par[A]) = ParOps[A](p)

  /** Primitive combinators. */
  def delay[A](a: => Par[A]): Par[A]
  def fork[A](a: => Par[A]): Par[A]
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
  def unit[A](a: A): Par[A]

  /** Derived combinators. */
  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    if (ps.isEmpty) unit(List.empty) else map2(ps.head, sequence(ps.tail))(_::_)
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val bs: List[Par[B]] = as map asyncF(f)
    sequence(bs)
  }
  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val ps: List[Par[List[A]]] =
      as map (asyncF((a: A) => if (p(a)) List(a) else List.empty))
    map(sequence(ps))(_.flatten)
  }

  /** Infixable operations. */
  case class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = self.map(p)(f)
  }
}
