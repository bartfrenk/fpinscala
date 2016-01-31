package fpinscala.par

import java.util.concurrent.TimeUnit

/** Utility functions for measuring computation time. */
object Time {
  /** Returns the value of a computation along with its duration. */
  def duration[A](block: => A, units: TimeUnit): (A, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, units.convert(t1 - t0, TimeUnit.NANOSECONDS))
  }

  /** Evaluates a computation and prints its duration. */
  def time[A](block: => A): A = {
    val (result, t) = duration(block, TimeUnit.MICROSECONDS)
    println(s"Elapsed time: $t us")
    result
  }
}
