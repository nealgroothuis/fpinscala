package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (randomNumber, newRNG) = rng.nextInt
    if (randomNumber > 0) then (randomNumber, newRNG)
    else if (randomNumber == Int.MinValue) then (0, newRNG)
    else (-randomNumber, newRNG)

  def double: Rand[Double] = map(int)(i =>
    (i.toDouble - Int.MinValue.toDouble) / (Int.MaxValue.toDouble - Int.MinValue.toDouble + 1.0)
  )

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r1) = double(rng)
    val (i, r2) = rng.nextInt
    ((d, i), r2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = rng0 =>
    rs.foldRight((List.empty[A], rng0)) { case (ra, (as, rng)) =>
      val (a, nextRng) = ra(rng)
      (a :: as, nextRng)
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng =>
    val (a, rng1) = r(rng)
    f(a)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i =>
    val mod = i % n
    if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
  )

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = s =>
      val (a, s1) = underlying(s)
      (f(a), s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = s =>
      val (a, s1) = underlying(s)
      val (b, s2) = sb(s1)
      (f(a, b), s2)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
      val (a, s1) = underlying(s)
      f(a)(s1)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = s0 =>
    sas.foldLeft((List.empty[A], s0)) { case ((as, s), sa) =>
      val (a, nextS) = sa(s)
      (as :+ a, nextS)
    }

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.sequence(
        inputs.map(input => State.modify(modifyFnForInput(input)))
      )
      finalState <- State.get
    yield (finalState.coins, finalState.candies)

  def modifyFnForInput(input: Input): Machine => Machine = (s: Machine) =>
    if s.candies > 0 then
      input match
        case Input.Coin if s.locked =>
          Machine(locked = false, candies = s.candies, coins = s.coins + 1)
        case Input.Turn if !s.locked =>
          Machine(locked = true, candies = s.candies - 1, coins = s.coins)
        case _ => s
    else s
