package chapter3

sealed trait Tree[+A]
case class Leaf[+A](value:A) extends Tree[A]
case class Branch[+A](left:Tree[A],right:Tree[A]) extends Tree[A]


object Tree {

  /**
    * 3.25
    * @param t
    * @tparam A
    * @return
    */
  def size[A](t:Tree[A]):Int={
    t match {
      case Leaf(_)=>1
      case Branch(l,r)=>size(l)+size(r)+1
    }
  }

  /**
    * 3.26
    * @param t
    * @return
    */
  def maximum(t:Tree[Int]):Int= {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }
  }

  /**
    * 3.27
    * @param t
    * @tparam A
    * @return
    */
  def depth[A](t:Tree[A]):Int={
    t match {
      case Leaf(_)=>1
      case Branch(l,r)=>depth(l).max(depth(r))+1
    }
  }

  /**
    * 3.28
    * @param t
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A,B](t:Tree[A])(f:A=>B):Tree[B]={
    t match {
      case Leaf(v)=>Leaf(f(v))
      case Branch(l,r)=>Branch(map(l)(f),map(r)(f))
    }
  }

  /**
    * 3.29
    * 1.思考为什么z是这个函数形式
    *   叶子节点===链表的尾节点（Nil）==>初始值
    * 2. 为什么f是这个函数
    *   f负责处理结构信息
    * @param t
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def fold[A,B](t:Tree[A])(z:A=>B)(f:(B,B)=>B):B={
    t match {
      case Leaf(v)=>z(v)
      case Branch(l,r)=> f(fold(l)(z)(f),fold(r)(z)(f))
    }
  }
  def main(args: Array[String]): Unit = {
    val t1=Branch(Branch(Leaf(9),Leaf(200)),Leaf(2))
    val t2=Branch(Branch(Leaf("test"),Leaf(200)),Leaf(2))
    println(t1,t2)

    println(size(t1))
    println(fold(t1)(_=>1)(_+_+1))

    println(maximum(t1))
    println(fold(t1)(identity)(_ max _))

    println(depth(t1))
    println(fold(t1)(_=>1)((l,r)=>(l max r) + 1))

    println(map(t1)(_+1))
    println(fold(t1)(v=>Leaf(v + 1):Tree[Int])((l,r)=>Branch(l,r)))
    def map_Fold[A,B](t:Tree[A])(f:A=>B):Tree[B]={
      fold(t)(a=>Leaf(f(a)):Tree[B])((l,r)=>Branch(l,r))
    }
    println(map_Fold(t1)(_+1))
  }

}
