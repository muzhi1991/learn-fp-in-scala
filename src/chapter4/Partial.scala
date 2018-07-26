package chapter4

trait Partial[+A,+B]{
  def map[C](f:B=>C):Partial[A,C]={
    this match {
      case Errors(e)=>Errors(e)
      case Success(v)=>Success(f(v))
    }
  }

  /**
    * 注意：因为flatMap的语义，在this为Error时，还是不会执行f
    * @param f
    * @tparam AA
    * @tparam C
    * @return
    */
  def flatMap[AA>:A,C](f: B=>Partial[AA,C]):Partial[AA,C]= {
    this match {
      case Errors(e) => Errors(e)
      case Success(v) => f(v)
    }
  }

  /**
    * map2只对两个都是Error的情况处理
    * @param b
    * @param f
    * @tparam AA
    * @tparam C
    * @tparam D
    * @return
    */
  def map2[AA>:A,C,D](b:Partial[AA,C])(f:(B,C)=>D):Partial[AA,D]={
    (this,b) match {
      case (Errors(e1),Errors(e2)) => Errors(e1 ++ e2)
      case _=>this.flatMap(aa=>b.map(bb=>f(aa,bb)))
    }

  }


}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {

  case class Person(name:Name,age:Age)
  sealed class Name(value:String)
  sealed class Age(value:Int)

  def mkName(name:String):Partial[String,Name]={
    if (name=="" || name==null) {
      Errors(Seq("Name is empty"))
    }else{
      Success(new Name(name))
    }
  }

  def mkAge(age:Int):Partial[String,Age]={
    if (age<0) Errors(Seq("Age is out of range"))
    else Success(new Age(age))
  }

  def mkPerson(name:String,age:Int):Partial[String,Person]={
    // 这个可以
        mkName(name).map2(mkAge(age))(Person(_,_))
    // 不可行，本质上for语句就是flapMap+map的调用
//    for(a<-mkName(name);b<-mkAge(age)) yield Person(a,b)
  }


  def main(args: Array[String]): Unit = {
    println(mkPerson("",-1))
  }

}
