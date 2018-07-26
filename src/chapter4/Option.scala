package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case _ => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
    //    this match {
    //      case Some(v)=>f(v)
    //      case _=>None
    //    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case _ => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)) getOrElse ob
    //    this match {
    //      case Some(v) => Some(v)
    //      case _ => ob
    //    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }
  }
}

case class Some[+A](v: A) extends Option[A]

case object None extends Option[Nothing]

object Option {



  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * 4.2
    * 多个步骤都可能出现异常（Option）时，使用flatMap
    * @param xs
    * @return
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
    * 把任意一个 f:A=>B的函数转换成Option处理的函数
    * 输入Option 输出 Option：方便链式处理是的异常中断
    * 如何理解 _ map f :
    *   1. 这边 _ 表示匿名函数 x=>x map f
    *   2. 根据返回值（Option[A] => Option[B] ）自动类型推导了
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    //    (a:Option[A])=> a map f
    _ map f

  }

  /**
    * 4.3
    * 多个option作为函数f参数，任何一个失败/none都不返回值
    */
  def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B)=>C):Option[C]={
    a.flatMap(aa=>b.map(bb=>f(aa,bb)))
//    a.flatMap(aa=>b.flatMap(bb=>Some(f(aa,bb))))
  }

  /**
    * 模仿lift写的两个参数的函数f的提升
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def lift2[A,B,C](f:(A,B)=>C):(Option[A],Option[B])=>Option[C]={
    (a,b)=>{
      a.flatMap(aa=>b.map(bb=>f(aa,bb)))
    }
  }

  /**
    * 4.4
    * @param a
    * @tparam A
    * @return
    */
  def sequence[A](a:List[Option[A]]):Option[List[A]]={
    a.foldLeft(Some(Nil):Option[List[A]])((acc,x)=>acc.flatMap(aa=>x.map(bb=>bb::aa)))
  }

  def sequence_my[A](a:List[Option[A]]):Option[List[A]]={
    a.foldLeft(Some(Nil):Option[List[A]])((acc,x)=>acc.flatMap(aa=>x.map(bb=>bb::aa).orElse(acc)))
  }

  def sequence_2[A](a:List[Option[A]]):Option[List[A]]={
    a match{
      case Nil=>Some(Nil)
      case h::t=>map2(h,sequence_2(t))(_ :: _)
    }
  }

  /**
    * 4.5 对list进行遍历，任何一个结果返回None，整个结果就是none
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A,B](a:List[A])(f:A=>Option[B]):Option[List[B]]={
    a.foldRight(Some(Nil):Option[List[B]])((x,acc)=>map2(f(x),acc)(_ :: _))
  }

  def main(args: Array[String]): Unit = {
   val optMax=lift2(math.max)

    println(optMax(None,Some(2)))

  }

}

