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
      * Exercise 1: Pascal’s Triangle
      */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

    /**
      * Exercise 2: Parentheses Balancing
      */
    def balance(chars: List[Char]): Boolean = {

        def countBrackets(chars: List[Char], open: Int): Boolean = {
            if (open < 0) false
            else if (chars.isEmpty) {
                if (open == 0) true else false
            }
            else if (chars.head == '(') countBrackets(chars.tail, open + 1)
            else if (chars.head == ')') countBrackets(chars.tail, open - 1)
            else countBrackets(chars.tail, open)
        }

        countBrackets(chars, 0)

    }

    /**
      * Exercise 3: Counting Change
      */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (coins.isEmpty || money < 0) 0
        else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

}
