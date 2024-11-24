package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _                   => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) =>
      if p(a) then Cons(() => a, () => b) else Empty
    )

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Option(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, bs) => Cons(() => f(a), () => bs))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, as) =>
      if f(a) then Cons(() => a, () => as) else as
    )

  def append[B >: A](r: LazyList[B]): LazyList[B] =
    foldRight(r)((a, bs) => Cons(() => a, () => bs))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, bs) => f(a).append(bs))

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll(s => s._1 == s._2)

  def mapViaUnfold[B](f: A => B): LazyList[B] = LazyList.unfold(this)(s =>
    s match
      case Empty      => Option.empty
      case Cons(h, t) => Option((f(h()), t()))
  )

  def takeViaUnfold(n: Int): LazyList[A] = LazyList.unfold((this, n))(s =>
    s match
      case (Cons(h, t), i) if i > 0 => Option(h(), (t(), i - 1))
      case _                        => Option.empty
  )

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this)(s =>
      s match
        case Cons(h, t) if p(h()) => Option((h(), t()))
        case _                    => Option.empty
    )

  def zipWith[B, C](b: LazyList[B], f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, b))(s =>
      s match
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Option((f(h1(), h2()), (t1(), t2())))
        case _ => Option.empty
    )

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that))(s =>
      s match
        case (Empty, Empty) => Option.empty
        case (Cons(h, t), Empty) =>
          Option(((Option(h()), Option.empty), (t(), Empty)))
        case (Empty, Cons(h, t)) =>
          Option(((Option.empty, Option(h())), (Empty, t())))
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Option(((Option(h1()), Option(h2())), (t1(), t2())))
    )
object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, LazyList.continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, LazyList.from(n + 1))

  lazy val fibs: LazyList[Int] =
    def loop(a: Int, b: Int): LazyList[Int] =
      LazyList.cons(a, loop(b, a + b))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).fold(LazyList.empty)((a, nextState) =>
      LazyList.cons(a, unfold(nextState)(f))
    )

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))(state => Option(state._1, (state._2, state._1 + state._2)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(i => Option(i, i + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(s => Option(a, a))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
