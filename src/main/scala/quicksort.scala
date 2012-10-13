package implementations


  def main(args: Array[String]) {
      val numbers = Array(3, 2)
    //l numbers = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    for (x <- numbers) print(x)
    sort(numbers)
    println()
  }

  def sort(numbers: Array[Int]) {

    if (numbers.length > 1) quicksort(0, numbers.length - 1)

    def swap(i: Int, j:Int) {
      val x = numbers(i)

      numbers(i) = numbers(j)
      numbers(j) = x
    }

    def quicksort(l: Int, r: Int) {
      if (l < r) {
        val s = hoarePartition(l,r)
            for (x <- numbers) print(x); println()

        quicksort(l, s - 1)
        quicksort(s + 1, r)
      }
    }

    def hoarePartition(left: Int, right: Int): Int = {
      val partition = numbers(left)
      var i = left
      var j = right
      while(i < j) {
        while (numbers(i) < partition) i += 1
        while (numbers(j) > partition) j -= 1
        swap(i,j)
      }
      swap(i,j)
      swap(left,j)
      j

    }
  }
}
