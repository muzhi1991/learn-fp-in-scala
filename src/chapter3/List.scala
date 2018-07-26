package chapter3

sealed trait List[+A]


case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }

  }

  /**
    * 3.2
    * @param l
    * @tparam A
    * @return
    */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail on empty list")
      case Cons(_, t) => t
    }

  }

  /**
    * 3.3
    * @param l
    * @param data
    * @tparam A
    * @return
    */
  def setHead[A](l: List[A], data: A): List[A] = {
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(data, t)
    }
  }

  /**
    * 3.4
    * @param l
    * @param n
    * @tparam A
    * @return
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

  }

  //  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  //    //    l match {
  //    //      case Nil=>Nil
  //    //      case Cons(h,t)=>{
  //    //        if(f(h))
  //    //          dropWhile(t,f)
  //    //        else
  //    //          l
  //    //      }
  //    //    }
  //    l match {
  //      case Cons(h, t) if f(h) => dropWhile(t, f)
  //      case _ => l
  //    }
  //  }


  /**
    * 3.5
    * 使用柯里化协助类型推导
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }
  }

  /**
    * 3.6
    * 去尾操作就很低效
    *
    * @param l
    * @tparam A
    * @return
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  /**
    * 3.9
    * @param l
    * @tparam A
    * @return
    */
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => y + 1)
  }

  /**
    * foldRight不能用尾递归（容易stackoverflow），也不能提前短路（如乘法*0）
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f)) // 注意这里先调用了foldRight递归，然后才会用f计算
    }
  }

  /**
    * 3.10
    * foldLeft可以用尾递归实现
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f) // 这里像是Loop
    }
  }



  /**
    * 3.11
    * @param l
    * @return
    */
  def sum3(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  /**
    * 3.11
    * @param l
    * @return
    */
  def product3(l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  /**
    * 3.9
    * @param l
    * @tparam A
    * @return
    */
  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  /**
    * 3.12 反向列表，对于与foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _))是正向操作
    * 复制了整个列表
    * @param l
    * @tparam A
    * @return
    */
  def reverse[A](l:List[A]):List[A]={
    foldLeft(l,Nil:List[A])((acc,x)=>Cons(x,acc))
  }

  /**
    * 3.13
    * 关键点：
    * 1. 使用函数对象：(b:B)=>b 形成调用链式，做后acc返回的是一个可以被调用的函数，参数时b
    * 2. 调用链顺序：（最后用z调用函数对象时）包含在内部的函数先调用，
    *    * foldRight先递归到最右端
    *    * 所以用(x,acc)=>(b:B)=>acc(f(b,x))，从右往左把老函数acc裹到最外面，最后调用，而不是(x,acc)=>(b:B)=>f(acc(b),x)
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldLeft_FoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as,(b:B)=>b)((x,acc)=>(b:B)=>acc(f(b,x)))(z)
//    foldRight(as,(b:B)=>b)((x,acc)=>(b:B)=>f(acc(b),x))(z)
  }

  /**
    * 3.13答案解答
    * @param as
    * @param outerIdent
    * @param combiner
    * @tparam A
    * @tparam B
    * @return
    */
  // Here is the same function with much more description
  def foldLeftViaFoldRight_1[A,B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {

    // foldLeft processes items in the reverse order from foldRight.  It's
    // cheating to use reverse() here because that's implemented in terms of
    // foldLeft!  Instead, wrap each operation in a simple identity function to
    // delay evaluation until later and stack (nest) the functions so that the
    // order of application can be reversed.  We'll call the type of this
    // particular identity/delay function BtoB so we aren't writing B => B
    // everywhere:
    /**
      * 延迟函数：BtoB
      */
    type BtoB = B => B

    // Here we declare a simple instance of BtoB according to the above
    // description.  This function will be the identity value for the inner
    // foldRight.
    def innerIdent:BtoB = (b:B) => b

    // For each item in the 'as' list (the 'a' parameter below), make a new
    // delay function which will use the combiner function (passed in above)
    // when it is evaluated later.  Each new function becomes the input to the
    // previous function (delayFunc).
    //
    //                        This much is just the type signature
    //                  ,-------^-------.
    def combinerDelayer:(A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b:B) => delayFunc(combiner(b, a))
    // `----------v---------'    `----------------v---------------'
    //         Paramaters                 The returned function

    // Pass the original list 'as', plus the simple identity function and the
    // new combinerDelayer to foldRight.  This will create the functions for
    // delayed evaluation with an combiner inside each one, but will not
    // apply any of those functions.
    def go:BtoB = foldRight(as, innerIdent)(combinerDelayer)

    // This forces all the evaluations to take place
    go(outerIdent)
  }

  /**
    * 3.13
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight_FoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as,(b:B)=>b)((acc,x)=>(b:B)=>acc(f(x,b)))(z)
    //    foldRight(as,(b:B)=>b)((x,acc)=>(b:B)=>f(acc(b),x))(z)
  }

  /**
    * 3.13
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight_Reverse[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as),z)((acc,x)=>f(x,acc))
    //    foldRight(as,(b:B)=>b)((x,acc)=>(b:B)=>f(acc(b),x))(z)
  }

  /**
    * 3.14
    * @param l1
    * @param l2
    * @tparam A
    * @return
    */
  def append_FoldRight[A](l1:List[A],l2:List[A]):List[A]={
    foldRight(l1,l2)(Cons(_,_))
  }

  /**
    * 3.15 书上用的concat2，思想一样，都是先跑到最后边然后开始合并
    * 总结可以使用foldRight代替递归！！！！！！！---》 即使不是尾递归！！！
    * @param as
    * @tparam A
    * @return
    */
  def concat[A](as:List[A]*):List[A]={
    as.length match {
      case 1=>as.head
      case _=>append(as.head,concat(as.tail:_*))
    }
  }

  def concat2[A](l:List[List[A]]):List[A]={
   foldRight(l,Nil:List[A])(append)
  }

///////////////////////////////////////////////////////////


  /**
    * 3.16
    * @param l
    * @return
    */
 def listPlus1(l:List[Int]):List[Int]={
   l match {
     case Nil=>l
     case Cons(h,t)=>Cons(h+1,listPlus1(t))
   }
 }
  def listPlus1_FoldRight(l:List[Int]):List[Int]={
    foldRight(l,Nil:List[Int])((x,acc)=>Cons(x+1,acc))
  }

  /**
    * 3.17
    * @param l
    * @return
    */
  def listToString_FoldRight(l:List[Double]):List[String]={
    foldRight(l,Nil:List[String])((x,acc)=>Cons(x.toString,acc))
  }

  /**
    * 3.18
    * map的实现
    * 函数库用的map_2的方法
    * @param l
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A,B](l:List[A])(f:A=>B):List[B]={
    foldRight(l,Nil:List[B])((x,acc)=>Cons(f(x),acc))
  }


  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
    * 3.19
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def filter[A](l:List[A])(f:A=>Boolean):List[A]={
    foldRight(l,Nil:List[A])((x,acc)=>if(f(x)) Cons(x,acc) else acc )
  }

  def filter_2[A](l:List[A])(f:A=>Boolean):List[A]={
    val buf= new collection.mutable.ListBuffer[A]
    def go(l:List[A]):Unit={
      l match {
        case Nil=>()
        case Cons(h,t)=>if(f(h)) buf+=h;go(t)
      }
    }
    go(l)
    List(buf:_*)
  }

  /**
    * 3.20
    * @param l
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def flat_map[A,B](l:List[A])(f:A=>List[B]):List[B]={
    foldRight(l,Nil:List[B])((x,acc)=>append(f(x),acc))
  }

  def flat_map_2[A,B](l:List[A])(f:A=>List[B]):List[B]={
    concat2(map(l)(f))
  }

  /**
    * 3.21
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def filter_FlatMap[A](l:List[A])(f:A=>Boolean):List[A]={
    flat_map(l)(x=>if(f(x)) List(x) else Nil )
  }

  /**
    * 3.22
    * @param l1
    * @param l2
    * @return
    */
  def addPair(l1:List[Int],l2:List[Int]):List[Int]={
    (l1,l2)match {
      case (Nil,_)=>Nil
      case (_,Nil)=>Nil
      case (Cons(h1,t1),Cons(h2,t2))=>Cons(h1+h2,addPair(t1,t2))
    }
  }

  /**
    * 3.23
    * @param l1
    * @param l2
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def zipWith[A,B,C](l1:List[A],l2:List[B])(f:(A,B)=>C):List[C]={
    (l1,l2)match {
      case (Nil,_)=>Nil
      case (_,Nil)=>Nil
      case (Cons(h1,t1),Cons(h2,t2))=>Cons(f(h1,h2),zipWith(t1,t2)(f))
    }
  }

  /**
    * 3.24
    * @param sup
    * @param sub
    * @tparam A
    * @return
    */
  @annotation.tailrec
  def hasSubsequence[A](sup:List[A],sub:List[A]):Boolean={
    @annotation.tailrec
    def startWith[A](l1:List[A],l2:List[A]):Boolean={
      (l1,l2) match {
        case (Nil,_) => false
        case (_,Nil) => true
        case (Cons(h1,t1),Cons(h2,t2))=> h1==h2 && startWith(t1,t2)

      }
    }

    if(startWith(sup,sub))
      true
    else
      sup match {
        case Nil =>false
        case Cons(h,t)=> hasSubsequence(t,sub)
      }

  }


  def main(args: Array[String]): Unit
  = {
    val ll = List("123", "1", "x")
    println(tail(ll), setHead(ll, "test"))
    println(drop(ll, 2))
    println(dropWhile(List(1, 2, 3, 4, 5))(x => x < 3))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)

    println(init(ll))

    println(foldRight(List(1, 2, 3, 4, 5), 0)(_ + _))
    println(foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _))

    /**
      * 3.8
      * 这里需要Nil:List[Int]，表示Nil不是List[Nothing]。或者用List[Int]()
      */
    println(foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))

    /**
      * 3.9 foldRight的应用
      */
    println(length(List(1, 2, 3, 4, 5)))

    println(reverse(List(1, 2, 3, 4, 5)))

    println(append_FoldRight(List(1, 2, 3, 4, 5),List(8,9)))
    println(concat(List(1, 2, 3, 4, 5),List(8,9),List(8,9)))

/////////////////////////////////////////
    println(listPlus1(List(1, 2, 3, 4, 5)))
    println(filter_2(List(1, 2, 3, 4, 5))(_%2==0))
    println(flat_map(List(1, 2, 3, 4, 5))(i=>List(i,i)))
    println(addPair(List(1, 2, 3, 4, 5),List(1, 2, 3, 4, 5)))

    println(hasSubsequence(List(1, 2, 3, 4, 5),List(3,3)))

    Vector().init

  }


}



