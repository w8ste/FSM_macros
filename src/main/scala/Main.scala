object Main:
  def main(args: Array[String]): Unit =
    val result = UppercaseMacro.toUpperCase("hello world")
    println(result) // prints "HELLO WORLD"
