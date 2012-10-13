package implementations

import org.specs2.mutable.Specification

class QuicksortTest Specification with Algorithms {
  "quicksort" should {
    "sort a list of integers using quicksortHead" in {
      val unsorted = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
      quicksortHead(unsorted) mustEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
    "sort a list of integers using quicksortMid" in {
      val unsorted = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
      quicksortMid(unsorted) mustEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }
}
