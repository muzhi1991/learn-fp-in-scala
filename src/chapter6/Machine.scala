package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  import State._

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
    /**
      * 理解点：
      * 1. 为什么需要.reverse:和sequence中foldLeft版本类似，这里组织的是函数链，为了顺序执行函数，需要正向连起来，连接的代码参考s2.map2(acc)((s,a)=>a)，如果对他进行run，先调用了s2，所有这里要reverse
      * 2. 为什么不能acc.map2(s2)((a,s)=>a)：从调用顺序思考（记住永远把acc放后面）
      * 3. 这里的List[Unit]，其实就是我们没有用总结的产出值而已，值修改了状态，所以只有传递状态就行。（也是modify没有方法值，只修改状态）
      *
      */
    val res = inputs.reverse.foldLeft(unit[Machine, List[Unit]](Nil))((acc, x) => {

      val s2 = modify[Machine](s => {
        (s, x) match {
          case (Machine(true, candy, coin), Coin) =>
            Machine(false, candy, coin + 1)
          case (Machine(true, _, _), Turn) =>
            s
          case (Machine(false, candy, coin), Turn) =>
            Machine(true, candy - 1, coin)
          case (Machine(false, _, _), Coin) =>
            s
        }
      })
      // 关联前后的状态
      s2.map2(acc)((s,a)=>a)
//      State[Machine, List[Unit]](s => s2.run(s) match {
//        case (a, b) =>
//          (Nil, b)
//      })
      //      unit((s2._2.coins,s2._2.candies))
    })

    /**
      * 这个用法也很有意思，get获取了flatMap隐含的状态。。
      */
    res.flatMap(_ => get.map(s2 => (s2.coins, s2.candies)))
  }

  /**
    * 柯里化函数定义，输入Input,输出(Machine=>Machine)函数
    * 每个输入对应一个状态转化函数
    *
    * @return
    */
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) =>
        s
      case (Coin, Machine(false, _, _)) =>
        s
      case (Turn, Machine(true, _, _)) =>
        s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence_FoldRight(inputs map (input => ((modify[Machine] _).compose(update)) (input))) // _ 表示把modify变成函数对象
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    val tmp = modify[Machine] _ compose update

    val input = List(Coin, Turn)
    val res1 = simulateMachine(input).run(Machine(true, 5, 10))
    val res2 = simulate(input).run(Machine(true, 5, 10))

    println(res1)
    println(res2)
  }

}
