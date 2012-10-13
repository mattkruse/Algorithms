package implementations


object maxmin {
  def Run(numbers: Array[Int]): Pair[Int, Int] = {

    val n = numbers.length

    if (n == 1) return Pair(numbers(1), numbers(1))
    else if (n == 2)
      if (numbers(0) > numbers(1)) return Pair(numbers(0), numbers(1))
      else return Pair(numbers(1), numbers(0))

      else return Pair(5,5)
  }
}
