package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e)  => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => Right(a)
    case _        => b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(br => f(a, br)))

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    @annotation.tailrec
    def loop(bs: List[B], as: List[A]): Either[E, List[B]] = as match
      case Nil => Right(bs)
      case h :: t =>
        f(h) match
          case Right(b) => loop(b :: bs, t)
          case Left(e)  => Left(e)
    loop(Nil, es).map(_.reverse)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] = (a, b) match
    case (Left(aes), Right(_))  => Left(aes)
    case (Right(_), Left(bes))  => Left(bes)
    case (Left(aes), Left(bes)) => Left(aes ++ bes)
    case (Right(a), Right(b))   => Right(f(a, b))

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, b) =>
      map2All(f(a), b, _ :: _)
    )

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] = traverseAll(as, identity)
