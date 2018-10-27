sealed trait ListToLearn[+A]

case object Nil extends ListToLearn[Nothing]
case class Cons[+A](head: () => A, tail: () => ListToLearn[A]) extends ListToLearn[A]

def listToLearn[A](list: A*): ListToLearn[A] = {
  new Cons[A](() => list.head, () => list.tail) {}
}

val test = new ListToLearn[Int]{1, 2, 3}
//val newList = test.

object ListToLearn {

  def cons[A](hd: => A, tl: => TestStream[A]): ListToLearn[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def sum(ints: ListToLearn[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: ListToLearn[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }
}