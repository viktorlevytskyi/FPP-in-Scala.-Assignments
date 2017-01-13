package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) true
      else {
        val newNumOpens =
          if (chars.head == '(') numOpens + 1
          else if (chars.head == ')') numOpens - 1
          else numOpens
        if (newNumOpens < 0) false
        else check(chars.tail, newNumOpens)
      }
    }

    check(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    var numWays = 0

    def recount(money: Int, coins: List[Int]): Unit = {
      if (coins.nonEmpty) {
        val rest: Int = money - coins.head
        if (rest == 0) numWays += 1
        if (rest > 0) recount(rest, coins)
        recount(money, coins.tail)
      }
    }

    if (money == 0 || coins.isEmpty) 0
    else {
      recount(money, coins)
      numWays
    }
  }
}
