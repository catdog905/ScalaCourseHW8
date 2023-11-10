package task2

import cats.Eval

object EvalTricks {

  def fib(n: Int): Eval[BigInt] = {
    def fibTailRec(i: Int, prev: BigInt, current: BigInt): Eval[BigInt] = {
      Eval.defer {
        if (i <= 1) Eval.now(current)
        else fibTailRec(i - 1, current, prev + current)
      }
    }

    if (n <= 0) Eval.now(0) else fibTailRec(n, 0, 1)
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def loop(list: List[A], acc: Eval[B]): Eval[B] = list match {
      case head :: tail =>
        Eval.defer(loop(tail, acc)).map(fn(head, _))
      case Nil =>
        acc
    }

    loop(as, Eval.now(acc)).value
  }
}
