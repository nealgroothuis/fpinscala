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

trait Prop

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

object Gen:
  def unit[A](a: => A): Gen[A] = ???

  extension [A](self: Gen[A]) def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
