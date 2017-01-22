def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
        if (a > b) acc
        else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
}

sum(x => x * x)(2, 5)

def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
}

product(x => x * x)(1, 4)

def fact(n: Int): Int = product(x => x)(1, n)
fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}
mapReduce(x => x, (x, y) => x * y, 1)(1, 5)

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) = math.abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
        val next = f(guess)
        if (isCloseEnough(guess, next)) next
        else iterate(next)
    }
    iterate(firstGuess)
}
fixedPoint(x => 1 + x / 2)(1.0)
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
sqrt(2)