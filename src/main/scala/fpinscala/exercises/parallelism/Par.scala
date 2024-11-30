package fpinscala.exercises.parallelism

sealed trait Par[A]
object Par:
  extension [A](pa: Par[A])
     def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = ???
