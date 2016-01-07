package fpinscala

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // explicit forcing of the h thunk using h()
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }

  // requires evaluation of head (how to avoid?)
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      val hd = h()
      if (p(hd)) cons(hd, t().takeWhile(p))
      else t().takeWhile(p)
    }
  }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }

  // In the Coursera course there is a lecture about printing only the evaluated prefix
  // of a stream, I think.
  override def toString: String = this match {
    case Empty => "Stream()"
    // toString should leave the stream unevaluated
    case Cons(h, t) => "Stream(_, ...)"
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def nats(lo: Int = 0): Stream[Int] = cons(lo, nats(lo + 1))

  def natsE(lo: Int = 0): Stream[Int] = cons({println("*" + lo); lo}, natsE(lo + 1))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
