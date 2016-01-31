package fpinscala.stream

sealed trait Stream[+A] {
  import Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forall(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)


  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // explicit forcing of the h thunk using h()
  }

  def safeTail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => t()
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

  def takeUF(n: Int): Stream[A] = {
    type S = (Int, Stream[A])
    def g(s: S): Option[(A, S)] = s match {
      case (1, Cons(h, t)) => Some((h(), (0, empty)))
      case (n, Cons(h, t)) => if (n > 1) Some((h(), (n - 1, t()))) else None
      case _ => None
    }
    unfold((n, this))(g)
  }

  def takeWhileUF(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Empty => None
      case Cons(h, t) => {
        val hd = h()
        if (p(hd)) Some((hd, t())) else None
      }
  })

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs))({
      case ((Cons(a, ta), Cons(b, tb))) => Some((f(a(), b()), (ta(), tb())))
      case _ => None
    })

  def startsWith[B](bs: Stream[B]): Boolean =
    this zipAll bs takeWhile {
      case (a, Some(b)) => true
      case _ => false
    } forall {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  def head: A = this match {
    case Cons(h, _) => h()
    case _ => throw new Exception()
  }

  def scanRight[B](z: => B)(f: (A, => B) => B) =
    foldRight(Stream(z)) {
      case (a, b) => cons(f(a, b.head), b)
    }

  def tails(): Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case as @ Cons(h, t) => Some((as, t()))
    } append Stream(empty[A])

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs))({
      case ((Empty, Empty)) => None
      case (at, bt) => Some(((at.headOption, bt.headOption), (at.safeTail, bt.safeTail)))
    })

  def hasSubsequence[B](bs: Stream[B]): Boolean =
    tails exists (_ startsWith bs)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) =>
      cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def takeWhileFL(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else empty
    )

  def headOptionFL: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  // requires evaluation of head (how to avoid?)
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      val hd = h()
      if (p(hd)) cons(hd, t().takeWhile(p))
      else Empty
    }
  }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }

  def mapUF[B](f: A => B): Stream[B] = {
    def p(as: Stream[A]): Option[(B, Stream[A])] = as match {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
    unfold(this)(p)
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

  def natsL(lo: Int = 0): Stream[Int] = cons({println("*" + lo); lo}, natsL(lo + 1))

  def natsE(lo: Int = 0): Stream[Int] = Cons(() => {println("*" + lo); lo}, () => natsE(lo + 1))

  def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def fibsUF(a: Int, b: Int): Stream[Int] =
    cons(a, unfold((a, b)){case (x, y) => Some((y, (y, x + y)))})

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
