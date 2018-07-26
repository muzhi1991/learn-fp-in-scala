package chapter5

import Stream._

/**
  * List的惰性版本
  *
  * @tparam A
  */
sealed trait Stream[+A] {
  def headOption(): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n == 0 => Empty
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n == 0 => Cons(h, t)
      case Cons(h, t) => t().drop(n - 1)
      case _ => empty
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _ => empty
    }
  }

  /**
    * 重要方法：惰性版本，对比非惰性就是f:(A,=>B)=>B
    *
    * @param z
    * @param f
    * @tparam B
    * @return
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }
  }

  /**
    * 非惰性版本，对比一下
    * @param z
    * @param f
    * @tparam B
    * @return
    */
  def foldRightOld[B](z: B)(f: (A, B) => B): B = {
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRightOld(z)(f))
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((x, acc) => p(x) || acc)
  }

  /**
    * 5.4
    * @param p
    * @return
    */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((x, acc) => p(x) && acc)
  }

  /**
    * 5.5
    * @param f
    * @return
    */
  def takeWhile_foldRight(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((x, acc) => if (f(x)) cons(x, acc) else empty)
  }

  /**
    * 5.6
    * @return
    */
  def headOption_foldRight(): Option[A] = {
    foldRight(None: Option[A])((x, _) => Some(x))
  }

  /**
    * 5.7 基于foldRight实现常用函数，这些都具有良好的惰性特征：『一等循环』
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((x, acc) => cons(f(x), acc))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((x, acc) => if (f(x)) cons(x, acc) else acc)
  }

  /**
    * 为什么参数要是非严格求值
    *
    * @param s 注意他是Stream
    * @tparam B
    * @return
    */
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((x, acc) => cons(x, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((x, acc) => f(x).append(acc))
  }

  /**
    * 很有意思的方法，说让filter的含义是过滤整个链表，但是由于是惰性求值，我们只用了head，所以过滤出head之后就不会往后遍历了！！！！
    *
    * @param p
    * @return
    */
  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption()
  }

  def map_unfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h,t)=>Some(f(h()),t())
      case _ => None
    }
  }

  def take_unfold(n:Int):Stream[A]={
    unfold(this,n){
      case (Cons(h,t),n) if n > 0 => Some(h(),(t(),n-1))
      case _ => None
    }
  }

  def takeWhile_unfold(p:A=>Boolean):Stream[A]={
      unfold(this){
        case Cons(h,t) if p(h()) => Some(h(),t())
        case _ => None
      }
  }

  def zipWith[B,C](s2:Stream[B])(f:(A,B)=>C):Stream[C]={
    unfold((this,s2)){
      case (Cons(h1,t1),Cons(h2,t2))=>Some(f(h1(),h2()),(t1(),t2()))
      case _=>None
    }

  }

  /**
    * 5.13
    * @param s2
    * @tparam B
    * @return
    */
  def zipAll[B](s2:Stream[B]):Stream[(Option[A],Option[B])]={
    unfold((this,s2)){
      case (Cons(h1,t1),Cons(h2,t2))=>Some((Some(h1()),Some(h2())),(t1(),t2()))
      case (Cons(h1,t1),Empty)=>Some((Some(h1()),None),(t1(),empty[B]))
      case (Empty,Cons(h2,t2))=>Some((None,Some(h2())),(empty[A],t2()))
      case _ => None
    }

  }

  /**
    * 5.14
    * @param s2
    * @tparam A
    * @return
    */
  def startsWith[A](s2:Stream[A]):Boolean={
    zipAll(s2).forAll {
      case (Some(a),Some(b))=>a==b
      case (Some(a),None)=>true
      case _ => false
    }
  }

  def tails:Stream[Stream[A]]={
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    }.append(Stream(empty))

  }

  def hasSubsequence[A](s:Stream[A]):Boolean={
    tails.exists( _ startsWith s)
  }
  def scanRight[B](z:B)(f:(A,=>B)=>B):Stream[B]={
    // 这个不对？？效率低下
//    unfold(this.append(empty[A])) {
//      case Cons(h, t) => Some((Cons(h, t).foldRight(z)(f), t()))
//      case Empty => None
//    }

    // 这个也可以
//    foldRight(Stream(z))((x,acc)=>{
//      lazy val lazyacc=acc
//      cons(f(x,lazyacc.headOption().getOrElse(z)),lazyacc)
//    })

    foldRight((z,Stream(z)))((x,acc)=>{
      lazy val lazyacc=acc
      val newz=f(x,lazyacc._1)
      (newz,cons(newz,lazyacc._2))
    })._2
  }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val h1 = h
    lazy val t1 = t
    Cons(() => h1, () => t1)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  /**
    * 5.11 重要方法，可以基于他实现其他方法，如map take等
    * @param z
    * @param f
    * @tparam A
    * @tparam S
    * @return
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }



  def main(args: Array[String]): Unit = {
    val s = Stream(1, 2, 3, 4, 5)
    println(s.drop(1).toList)
    println(s.forAll(_ >= 4))
    println(s.takeWhile_foldRight(_ < 3).toList)
    println(s.headOption_foldRight())


    /**
      * 没有lazy编译不过。。
      *
      * @param a
      * @tparam A
      * @return
      */
    def constant[A](a: A): Stream[A] = {
      lazy val repeat: Stream[A] = Stream.cons(a, repeat)
      repeat
    }
    //    def constant[A](a: A): Stream[A] = {
    //      lazy val tail: Stream[A] = Cons(() => a, () => tail)
    //      tail
    //    }

    def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n + 1))
    }

    def fibs(): Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] = {
        Stream.cons(a, go(b, a + b))
      }

      go(0, 1)
    }


    def fibs_unfold() = {
      unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }
    }

    def from_unfold(n: Int) = {
      unfold(n)(s => Some(s + 1, s + 1))
    }

    def constant_unfold(n: Int) = {
      unfold(n)(s => Some(s, s))
    }

    def ones_unfold() = unfold(1)(_ => Some(1, 1))


    //
    val ones: Stream[Int] = constant(1)
    //   lazy val ones: Stream[Int] = Stream.cons(1, ones)
    println(from(19).take(5).toList)
    println(unfold(1)(s => Some((s, s * 2))).take(5).toList)

    println(s.takeWhile_unfold(_ <=2).toList)
    val s2 = Stream("x", "y", "z")
    println(s.zipWith(s2)((a,b)=>a.toString+b).toList)

    val s3 = Stream(1, 2, 3, 4)
    println(s.startsWith(s3))
    println(s.tails.toList.map(_.toList))

    println(s.hasSubsequence(Stream( 3, 4)))


    println(Stream( 1,2,3).scanRight(0)(_+_).toList)
  }


}
