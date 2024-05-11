package chapter6

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (value, newState) = run(s)
      f(value) -> newState
    })
    
  def map2[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (value1, newState1) = run(s)
      val (value2, newState2) = action2.run(newState1)
      f(value1, value2) -> newState2
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (value, newState) = run(s)
      f(value).run(newState)
    })
}

object State:
  def unit[S, A](a: A): State[S, A] = State(s => a -> s)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldLeft(State.unit(List[A]()))((acc, s) => acc.map2(s)(_ :+ _))

  // provided
  def get[S]: State[S, S] = State(s => (s, s))

  // provided
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // provided
  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()
