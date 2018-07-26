package chapter6

/**
  *
  * 泛化状态
  *
  *
  */

/**
  * 通用State的class实现
  * （type实现键下面 type State[S, +A] = S => (A, S)）
  * 使用class表示的好处：可以实现map和flatMap，就可以使用for语句
  *
  * @param run
  * @tparam S
  * @tparam A
  */
case class State[S, +A](run: S => (A, S)) {

  import State._
  //

  def map[B](f: A => B): State[S, B] = {

    //    State(s => {
    //      val r = this.run(s)
    //      (f(r._1), r._2)
    //    })

    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val r1 = this.run(s)
      val r2 = sb.run(r1._2)
      (f(r1._1, r2._1), r2._2)
    })

    //    flatMap(a=>sb.map(b=>f(a,b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val r = this.run(s)
      f(r._1).run(r._2)
    }
    )
  }


}


object State {
  /**
    * 代表对变量A操作引发的状态变化
    * 对比：type Rand[+A] = RNG => (A, RNG)
    *
    * @tparam S 这里的S对应于Rand中的RNG（随机数状态）
    * @tparam A 这里的A对应于Rand的A
    */
  //  type State[S, +A] = S => (A, S)
  // val t: State[Int, Int] = x => (x, x + 1)
  // 通过一个类型定义另外一个类型
  type Rand[+A] = State[RNG, A]


  // 对象State的run存储了这个函数S => (A, S)对象
  //  case class State[S,+A](run:S=>(A,S))
  //    case class Rand2[+A] extends State[RNG,A]
  //  val t = State((x:Int)=>(x,x+1))
  // t.run(1)

  def unit[S, A](a: A): State[S, A] = {
    State((a, _))
  }

  /**
    * 一个List的State顺序制性（先后），生成最终状态State
    *
    * @param rl
    * @tparam S
    * @tparam A
    * @return
    */
  def sequence[S, A](rl: List[State[S, A]]): State[S, List[A]] = {
    rl.reverse.foldLeft(unit[S, List[A]](List[A]())) { (acc, x) =>
      //      State(s=>{
      //        val r1=x.run(s)
      //        val r2=acc.run(r1._2)
      //        (r1._1 :: r2._1,r2._2)
      //      })
      x.map2(acc)(_ :: _)

    }
  }

  /**
    * 理解：下面三种实现是等价的---执行顺序一致，结果输出一致
    *
    */

  def sequence_FoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => { // unit是在构造函数，本质上是构造函数调用链-->懒加载
      println("here")
      f.map2(acc)((a, b) => { // map2是在构造函数，本质上是构造函数调用链-->懒加载
        println(f, a, b)
        a :: b
      })
    })

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence_Loop[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    // 最后构造函数
    State((s: S) => go(s, sas, List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequence_FoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _)) // unit,map2是在构造函数


  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = {
    get.flatMap(s1 => {
      val x = set(f(s1))
      //      println(f(s1),x.run)
      x
    })
  }

  def main(args: Array[String]): Unit = {

    //    val t: State[Int, Int] = x => (x, x + 1)
    //    val t = State((x: Int) => (x, x + 1))
    //    println(t.run(1))

    val int: Rand[Int] = State(rng => rng.nextInt)

    def ints_old(count: Int): Rand[List[Int]] = {
      State((rng: RNG) => {
        if (count > 0) {
          val (l, rng1) = ints_old(count - 1).run(rng)
          val (d, rng2) = rng1.nextInt
          (d :: l, rng2)
        } else {
          (Nil, rng)
        }
      })

    }

    def ints_loop(count: Int): Rand[List[Int]] = {
      sequence_Loop(List.fill(count)(int))

    }

    def ints_foldleft(count: Int): Rand[List[Int]] = {
      sequence_FoldLeft(List.fill(count)(int))

    }

    def ints_foldright(count: Int): Rand[List[Int]] = {
      sequence_FoldRight(List.fill(count)(int))

    }

    /**
      * 三种方式结果相同
      */
    println(ints_loop(10).run(SimpleRNG(1)))
    println(ints_foldleft(10).run(SimpleRNG(1)))
    println(ints_foldright(10).run(SimpleRNG(1)))

    println(int.run(SimpleRNG(1)))

    /**
      * 只有loop版本是正常运行的
      * stackoverflow
      */
    val ns: Rand[List[Int]] = int.flatMap(a => int.flatMap(b => ints_loop((a).toInt).map(xs => xs.map(_ % b))))
    //    println(ns.run(SimpleRNG(1)))

    /**
      * stackoverflow
      */
    // for 语句的返回值类型与最后一个<-的类型相同
    //   val xx= for(x<-int;y<-int;xs<-ints(x/10000))
    //      yield(xs.map(_%y))
    //
    //    println(xx.run(SimpleRNG(1)))
    //
    //  val yy=for(i<-List(1,2);j<-Map(9->4,8->5))
    //    yield (i,j)
    //    println(yy)
    //    println(ns.run(SimpleRNG(1)))

    println(modify((x: Int) => x + 100).run(1))
  }

}
