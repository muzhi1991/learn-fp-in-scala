package chapter2

/**
  * 通过类型实现多态??
  */
object PolymorphicByType {

  /**
    * 部分应用函数，应用一部分已知的参数，如下面的f(a,b)只有a时，先应用a，函数编程了f(b)
    * 类似于闭包
    *
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /**
    * 柯里化，对函数形态的改变
    *
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {

    (a: A) => {
      (b: B) => f(a, b)
    }
  }


  /**
    * 注意：由于=>是右结合的操作符，A => B => C 与 A => (B => C) 等价。含义是输入是一个值，输出是一个函数
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => {
      f(a)(b)
    }
  }


  /**
    * 组合函数，从不同视角看
    * * 改变了函数f的输入（最常用的视角），在Scala中 f.compose(g)就是这个功能
    * * 改变了函数g的输出
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => {
      f(g(a))
    }
  }


}
