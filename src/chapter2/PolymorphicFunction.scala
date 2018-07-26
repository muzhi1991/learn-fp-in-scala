package chapter2

/**
  * 多态函数（也称为泛型函数），函数式编程特有的，有点类似C++泛型函数和Java的泛型方法，不是指的是面向对象中的多态，
  */
object PolymorphicFunction {
  /**
    * 我用等号实现的，因为key是泛型，理论上对key的操作只有『==』一个
    * @param ss
    * @param key
    * @tparam A
    * @return
    */
  def findFirst[A](ss:Array[A],key:A):Int={
    @annotation.tailrec
    def loop(n:Int):Int={
      if (n>=ss.length)
        -1
      else if (ss(n)== key)
        n
      else
        loop(n+1)
    }
    loop(0)
  }

  /**
    * 书中的实现，因为key是泛型，更通用的做法是把A的操作抽象为函数
    * @param ss
    * @param eq
    * @tparam A
    * @return
    */
  def findFirst[A](ss:Array[A],eq:A=>Boolean):Int={
    @annotation.tailrec
    def loop(n:Int):Int={
      if (n>=ss.length)
        -1
      else if (eq(ss(n)))
        n
      else
        loop(n+1)
    }
    loop(0)
  }

  /**
    * 练习2.2
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as:Array[A],ordered: (A,A)=>Boolean):Boolean={
    def loop(n:Int):Boolean= {
      if (n >= as.length)
        true
      else if (!ordered(as(n-1),as(n)))
        false
      else
        loop(n+1)

    }
    if (as.length==1)
      true
    else
      loop(1)
  }

  def main(args: Array[String]): Unit = {
    println(findFirst(Array("Mary", "had", "a", "little", "lamb"),"little"))
    println(findFirst(Array(1, 2, 3, 4),(x:Int)=>x==2))

    println(isSorted(Array(1, 2, 3, 4),(x:Int,y:Int)=>x<=y))
    List(1).init
  }

}
