package recfun

import common._

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
    assert(r >= 0 && c >= 0 && c <= r, s"invalid coordinates ($c, $r)")
    if (c == 0 || c == r) 1                       // edges
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceAux(expr: List[Char], state: Int): Boolean = {
      if (expr.isEmpty) state == 0
      else if (expr.head == '(') balanceAux(expr.tail, state + 1)
      else if (expr.head == ')') {
        if (state == 0) false                     // no '(' that still needs closing
        else balanceAux(expr.tail, state - 1)
      }
      else balanceAux(expr.tail, state)
    }
    balanceAux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
