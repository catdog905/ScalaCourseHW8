package task2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import task2.EvalTricks._

class EvalTricksSpec extends AnyFlatSpec with Matchers {
  "Fib function" should "correctly compute the first Fibonacci number" in {
    fib(0).value should be(0)
  }

  it should "correctly compute the second Fibonacci number" in {
    fib(1).value should be(1)
  }

  it should "correctly compute a mid-range Fibonacci number" in {
    fib(10).value should be(55)
  }

  it should "compute the nth Fibonacci number without stack overflow" in {
    noException should be thrownBy fib(1000000).value
  }

  "foldRight" should "correctly aggregate a list of integers" in {
    val list = List(1, 2, 3, 4, 5)
    val result = foldRight(list, 0)(_ + _)
    result should be(15)
  }

  it should "be stack safe for large lists" in {
    val largeList = List.fill(10000000)(1)
    val result = foldRight(largeList, 0)(_ + _)
    result should be(10000000)
  }
}
