object HelloWorld extends App {
  println("hello world asdf")

  case class Person(name: String, age: String)
  var p = Person("sixuan", "24")
  println(p)

}

object practice1 extends App{
  def curry[A,B,C](f: (A,B) => C): A => (B => C)= {
    a => b => f (a, b)
  }
}



object practice2dot1 {
  def fib(n: Int) : Int = {
    def loop(n: Int, n1: Int, n2: Int): Int = {
      if (n < 2)
        0
      else
        n1 + n2
      loop(n, n1, n2)
    }
    loop(n, 0, 1)
  }
}