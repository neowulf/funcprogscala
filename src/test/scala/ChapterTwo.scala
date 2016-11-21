import org.scalatest._
import org.scalatest.Matchers._

/**
  *
  * @author siva
  */
class ChapterTwo extends FlatSpec {

  "Chapter Two" should "2.1" in {
    // Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The
    // first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous
    // two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function.
    //    def fib(n: Int): Int

    def fib(n: Int): Int = {
      @annotation.tailrec
      def go(n: Int, acc1: Int, acc2: Int): Int = {
        if (n < 1) 0
        else if (n <= 2) acc1 + acc2
        else go(n - 1, acc2, acc1 + acc2)
      }
      go(n, 0, 1)
    }

    // 0 1 1 2 3 5 8 13
    fib(0) should be (0)
    fib(1) should be (1)
    fib(2) should be (1)
    fib(3) should be (2)
    fib(9) should be (34)
    fib(10) should be (55)
  }

  it should "2.2" in {
    // Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
    //  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @annotation.tailrec
      def go(as: Array[A], acc: Boolean): Boolean = {
        if (as.length == 2) ordered(as(0), as(1))
        else if (as.length < 2) true
        else go(as.tail, ordered(as.head, as.tail.head))
      }

      go(as, acc = true)
    }

    val f: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y

    assert(isSorted(Array(0, 1, 2, 3, 4), f))
    assert(isSorted(Array(0), f))
    assert(isSorted(Array(0, 1), f))
    assert(isSorted(Array(0, 1, 2), f))
    assert(!isSorted(Array(4, 1), f))
    assert(!isSorted(Array(0, 1, 2, 3, 4, 2), f))
  }

  it should "2.3" in {
    // Let’s look at another example, currying, which converts a function f of two arguments into a
    // function of one argument that partially applies f. Here again there’s only one implementation that
    // compiles. Write this implementation.
    //    def curry[A,B,C](f: (A, B) => C): A => (B => C)

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a,b)
    }
  }

  it should "2.4" in {
    // Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right,
    //  A => (B => C) can be written as A => B => C.
    //      def uncurry[A,B,C](f: A => B => C): (A, B) => C

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      // f(a) returns a new func where the arg is B. This new func returns C
      (a: A, b: B) => f(a)(b)
    }
  }

  it should "2.5" in {
    // Implement the higher-order function that composes two functions.
    //    def compose[A,B,C](f: B => C, g: A => B): A => C

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }
  }
}
