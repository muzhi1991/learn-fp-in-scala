package chapter2

/**
  * high of function
  */
object HOF {
  /**
    * n值过大（>>15） 因为int溢出(2147483647),n=100就是0
    * @param n
    * @return
    */
  def factorialNoTail(n: Int): Int = {
    if (n == 1) {
      1
    } else {
      factorialNoTail(n - 1) * n // 缺点：不能尾递归优化
    }
  }


  def factorial(n: Int): Int = {
    /**
      * annotation.tailrec注释，强制检测是否可以尾递归优化
      * @param n
      * @param acc
      * @return
      */
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0)
        acc
      else
        go(n - 1, acc * n) // 注意如果是 go(n - 1, acc * n)+1 则不能尾递归优化。

    }

    go(n,1)
  }

  def fibonacciNoTail(n:Int):Int={
    if (n == 1) {
      0
    }else if(n==2){
      1
    }else {
      fibonacciNoTail(n-1)+fibonacciNoTail(n-2)
    }
  }


  def fibonacci(n:Int):Int={
    @annotation.tailrec
    def go(n:Int,x1:Int,x2:Int):Int= {
      if (n == 1) {
        x1
      }else if(n==2){
        x2
      }else {
        go(n-1,x2,x1+x2)
      }
    }

    go(n,0,1)
  }
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 1) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  /**
    * 打印结果
    * @param name
    * @param n
    * @param f 高阶函数的例子
    * @return
    */
  def formatResult(name:String, n:Int,f:Int=>Int)={
    "The %s value of %d is %d ".format(name,n,f(n))
  }
  def main(args: Array[String]): Unit = {
    // 当n很大时，stackoverflow
    // println(factorialNoTail(100000))
    // 尾递归n很大就没事
    println(factorial(100000))

    println(factorialNoTail(10))
    println(factorial(10))
    println(fibonacciNoTail(7))
    println(formatResult("fib",7,fib))

  }

}
