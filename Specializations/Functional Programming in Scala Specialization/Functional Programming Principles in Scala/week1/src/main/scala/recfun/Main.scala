package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    countChange(0, List())
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r) 1  //either first column or last column
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(chars: List[Char], weight: Int): Boolean = {
        if(chars.isEmpty) weight == 0  //reached end
        else if(weight < 0) false // Out of balance detected
        else {
          def curChar = chars.head
          if (curChar == '(') balance(chars.tail, weight + 1)
          else if (curChar == ')') balance(chars.tail, weight - 1)
          else balance(chars.tail, weight)
        }
      }

    if(chars.isEmpty) throw new NoSuchElementException

    balance(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def choice(money: Int, coins: List[Int]): Int = {
        if(money == 0) 1 // path completed
        else if(money <= 0 || coins == Nil) 0  // not a valid path, end
        else choice(money - coins.head, coins) + choice(money, coins.tail)
      }

      if(money <= 0 || coins.isEmpty) 0
      else choice(money, coins)
    }

    def countChange2(money: Int, coins: List[Int]): Int = {
      def choice(money: Int, index: Integer): Int = {
        if(money == 0) 1 // path completed
        else if(money <= 0) 0  // not a valid path, end
        else if(index == coins.length) 0 //reached end of iteration level and no match
        else choice(money - coins(index), index) + choice(money, index+1)
      }

      if(money <= 0 || coins.isEmpty) 0
      else choice(money, 0)
    }
  }
