package chapter4

sealed trait Either[+E,+A]{
  def map[B](f:A=>B):Either[E,B]={
    this match {
      case Left(e)=>Left(e)
      case Right(v)=>Right(f(v))
    }
  }

  /**
    *
    * @param f
    * @tparam EE 必须使用EE>:E，如果直接用trait定义的E，f可能返回了E的子类，破坏了协变的约定：https://stackoverflow.com/questions/43180310/covariant-type-a-occurs-in-contravariant-position-in-type-a-of-value-a
    *            我们期望所有的错误都是一类的，但是又不能直接用类定义的类型E
    * @tparam B
    * @return
    */
  def flatMap[EE>:E,B](f: A=>Either[EE,B]):Either[EE,B]= {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }
  def orElse[EE>:E,B>:A](b: =>Either[EE,B]):Either[EE,B]={
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }
  }
  def map2[EE>:E,B,C](b:Either[EE,B])(f:(A,B)=>C):Either[EE,C]={
    this.flatMap(aa=>b.map(bb=>f(aa,bb)))
  }
}

case class Left[+E](value:E) extends Either[E,Nothing]
case class Right[+A](value:A) extends Either[Nothing,A]


object Either {

  def sequence[E,A](l:List[Either[E,A]]):Either[E,List[A]]={
    l.foldRight(Right(Nil):Either[E,List[A]])((x,acc)=>x.map2(acc)(_::_))
  }

  def traverse[E,A,B](l:List[A])(f:A=>Either[E,B]):Either[E,List[B]]={
    l.foldRight(Right(Nil):Either[E,List[B]])((x,acc)=>f(x).map2(acc)(_::_))
  }
  def main(args: Array[String]): Unit = {
    println(mkPerson("",-1))
  }

  case class Person(name:Name,age:Age)
  sealed class Name(value:String)
  sealed class Age(value:Int)

  def mkName(name:String):Either[String,Name]={
    if (name=="" || name==null) {
      Left("Name is empty")
    }else{
      Right(new Name(name))
    }
  }

  def mkAge(age:Int):Either[String,Age]={
    if (age<0) Left("Age is out of range")
    else Right(new Age(age))
  }

  def mkPerson(name:String,age:Int):Either[String,Person]={
//    mkName(name).map2(mkAge(age))(Person(_,_))
    for(a<-mkName(name);b<-mkAge(age)) yield Person(a,b)
  }



}




