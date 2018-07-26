package chapter4

import scala.util.Try

object Test {

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("failed")
    else
      xs.sum / xs.length

  class A{}
   class B extends A{}

  def main(args: Array[String]): Unit = {
    val a=Some(new B()).orElse(Some(new A()))
    val b=Some(new A()).orElse(Some(new B()))
    println(Try{1/1})
  }

}
