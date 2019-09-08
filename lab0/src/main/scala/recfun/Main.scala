package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println()
    println("Parantheses balancing")
    var string: String =
      "I told him (that it's not (yet) done).(But he wasn't listening)"
    println(string)
    println(balance(string.toList))
    println()
    println("Counting change")
    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    var result: Int = 0
    if (r == 0 || c == 0 || c == r) {
      result = 1
    } else {
      result = pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
    return result
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var opened = List[Char]()
    for (character <- chars) {
      if (character.equals('(')) {
        opened = character +: opened
      }
      if (character.equals(')')) {
        if (opened.length == 0) {
          return false
        } else if (opened.length != 0) {
          opened = opened.drop(1)
        }
      }
    }
    return true
  }

  /**
    * Exercise 3
    */
  var current: Int = 0
  def countChange(money: Int, coins: List[Int]): Int = {
    var count: Int = 0
    if (money == 0) {
      return 1
    }
    if (money < 0 || coins.isEmpty) {
      return 0
    } else {
      return countChange(money, coins.tail) + countChange(
        money - coins.head,
        coins
      )
    }
    return count
  }
}
