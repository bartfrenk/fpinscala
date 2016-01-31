package fpinscala.par

/** Implementation of 'Par' that sequences computations. */
object SynchronousImpl {

  object Par {
    /** Instantiates a block computation without evaluating it. */
    def apply[A](a: => A): Par[A] = new Par(() => a)
  }
  class Par[A](val get: () => A)

  /** Evaluates computation (if it is not yet evaluated) and returns the result. */
  def run[A](a: Par[A]): A = a.get()

  object Monad extends ParMonad[Par] {
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = Par(f(a.get(), b.get()))
    def fork[A](a: => Par[A]): Par[A] = a
    def unit[A](a: A): Par[A] = Par(a)
    def delay[A](a: => Par[A]): Par[A] = fork(a)

    def unitL[A](a: => A): Par[A] = Par(a)
  }
}
