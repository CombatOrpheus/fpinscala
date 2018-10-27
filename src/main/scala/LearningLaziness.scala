sealed trait TestStream[+A]

case object Empty extends TestStream[Nothing]
case class Cons[+A](h: () => A,
                    t: () => TestStream[A]
                   ) extends TestStream[A]

object TestStream {
  def cons[A](hd: => A,
              tl: => TestStream[A]): TestStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: TestStream[A] = Empty

  def apply[A](as: A*): TestStream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    }

  def head[A](stream: TestStream[A]): () => A ={
    stream match {
      case Cons(h, _) => h
    }
  }

  def tail[A](stream: TestStream[A]): TestStream[A] ={
    stream match{
      case Empty => Empty
      case Cons(_, t) => t()
    }
  }
}
