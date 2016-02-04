package fpinscala.par

import scala.language.higherKinds

/** Class that instantiates to examples for specific 'Par' implementations. */
class Examples[Par[_]](P: ParMonad[Par]) {

  import P._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  def sleep(n: Long): Par[Long] = lazyUnit({Thread.sleep(n); n})

  def sleepAndFail(n: Long): Par[Long] = lazyUnit(
    {Thread.sleep(n); throw new Exception(s"Failed after $n seconds")}
  )

  def sleep2(n: Long, m: Long): Par[Long] = map2(sleep(n), sleep(m))(_ + _)

  def sleep2AndFailOnFirst(n: Long, m: Long): Par[Long] = map2(sleepAndFail(n), sleep(m))(_ + _)

  def sleepSeq(ns: List[Long]): Par[List[Long]] = sequence(ns map sleep)


}
