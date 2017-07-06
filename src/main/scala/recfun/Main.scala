package recfun

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
    def opens(chars: List[Char], n: Int): Boolean = {
      if (chars.isEmpty && n == 0) true
      else if (chars.isEmpty && n != 0) false
      else if (n < 0) false
      else if (chars.head == '(') opens(chars.tail, n + 1)
      else if (chars.head == ')') opens(chars.tail, n - 1)
      else opens(chars.tail, n)
    }

    opens(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recur(m: Int, c: List[Int], i: Int): Int = {
      if(m < 0) i
      else if(m == 0) i + 1
      else if(c.isEmpty) i
      else recur(m, c.tail, i) + recur(m - c.head, c, i)
    }
    recur(money, coins, 0)
  }
}