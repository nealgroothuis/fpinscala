package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors, ExecutorService}

/** Exercise 8.1. Sum of a reversed (or generally, permuted) list is the same as
  * the original Sum of a zero-length list is 0. Sum of a list of n elements of
  * a is a*n.
  */

/* Exercise 8.2: Properties of max(List[Int])
 * - Max of an empty list is undefined
 * - Max of a single-element list is the element
 * - Max of a two-item list [a, b] is the greater of a and b
 * - Max of any list of two or more items is the max of the maxes of any partitioning of the list into non-empty sub-lists
 */

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop:
  self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(that: Prop): Prop =
    new Prop:
      override def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

opaque type Gen[+A] = State[RNG, A]
object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(_ + start))

  def unit[A](a: => A): Gen[A] = State(RNG.unit(a))
  def boolean: Gen[Boolean] = State(RNG.map(RNG.nonNegativeInt(_))(_ % 2 == 0))
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))
  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
  extension [A](self: Gen[A])
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(which => if which then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = State(
    RNG.double
  ).flatMap(d => if d < g1._2 / (g1._2 + g2._2) then g1._1 else g2._1)

trait SGen[+A]
