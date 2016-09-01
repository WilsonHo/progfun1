package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    countChange(300, List(500, 5, 50, 100, 20, 200, 10))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case c if c >= r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def processBalance(chars: List[Char], stack: List[Char]): Boolean =
      if (chars.isEmpty) stack.isEmpty
      else chars.head match {
        case '(' => processBalance(chars.tail, chars.head :: stack)
        case ')' => if (stack.contains('(')) processBalance(chars.tail, stack.dropRight(1)) else false
        case _ => processBalance(chars.tail, stack)
      }

    val stack = List[Char]()
    processBalance(chars, stack)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x >= 1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

}
