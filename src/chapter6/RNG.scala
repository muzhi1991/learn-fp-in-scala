package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  /**
    * Int.minValue Int.maxValue 之间的随机数（minValue的值的绝对值比maxValue还大1）
    *
    * @return
    */
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = new SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
//    println("now:"+n)
    (n, nextRNG)
  }


}

object RNG {

  /**
    * 下面实现的所有函数RNG本质上就是用来记录『seed』的（里面的方法nextInt并不重要）
    * @param rng
    * @return
    */

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    ((n1, n2), rng2)

  }

  /**
    * 0--Int.MaxValue的随机数
    *
    * @param rng
    * @return
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng1) = rng.nextInt
    val n = if (n1 < 0) -(n1 + 1) else n1
    (n, rng1)
  }

  /**
    * 0--1的double（不含1）
    *
    * @param rng
    * @return
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng1) = nonNegativeInt(rng)
    val n = n1 / (Int.MaxValue.toDouble + 1)
    (n, rng1)

  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = double(rng1)
    ((n1, n2), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n1, n2), rng1) = intDouble(rng)
    ((n2, n1), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (l, rng1) = ints(count - 1)(rng)
      val (d, rng2) = rng1.nextInt
      (d :: l, rng2)
    } else {
      (Nil, rng)
    }
  }

  /**
    * 这里RNG对象的目的也是记录状态Seed，用Long其实也ok
    * type Rand[+A]= Long=>(A,Long)
    * val int Rand[Int]= seed => nextInt(seed)  // 把nextInt参数Long改写成返回(Int,Long)
    * @tparam A
    */
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = rng => rng.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng1)
    }
  }

  /**
    * 被2整除的非负数字
    *
    * @return
    */
  def nonNegativeEven = map(nonNegativeInt)(i => i - i % 2)

  def double_map = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //        map(map(ra)(r1 => f(r1,r2)))(r2 =>( ))
    rng => {
      val (a1, rng1) = ra(rng)
      val (a2, rng2) = rb(rng1)
      (f(a1, a2), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def randIntDouble: Rand[(Int, Double)] = {
    both(int, double_map)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def sequence[A](rl: List[Rand[A]]): Rand[List[A]] = {
    //    rng => {
    //      rl match {
    //        case Nil => (Nil, rng)
    //        case _ =>
    //          val (a, rng1) = rl.head(rng)
    //          val (l, rng2) = sequence(rl.tail)(rng1)
    //          (a :: l, rng2)
    //      }
    //    }

    rl.foldRight(unit(Nil: List[A]))((x, acc) => {

      map2(x, acc)(_ :: _)
      //        rng=>{
      //          val(a,rng1)=x(rng)
      //          val (b,rng2)=acc(rng1)
      //          (a::b,rng2)
      //        }

    })
  }

  def ints_sequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /**
    * 6.8 重要，隐式传递了状态rng
    *
    * @param r
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng => {
      val (n, rng1) = r(rng)
      f(n)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def map_flatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { a =>
      unit(f(a))
    }
  }

  def map2_flatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb)(b => f(a, b))
      //      flatMap(rb) { b =>
      //        unit(f(a, b))
      //      }
    }
  }





  def main(args: Array[String]): Unit = {
    val (n, rng) = new SimpleRNG(42).nextInt
    println(n)
    val (n2, rng2) = rng.nextInt
    println(n2)

    println(Int.MaxValue + 1, Int.MaxValue)
    val x = List(double_map, double_map)
    val x2 = sequence(x)
    println(x2(new SimpleRNG(42)))


  }

}


