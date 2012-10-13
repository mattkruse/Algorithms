package implementations

import scala.util.Random

object Main extends Algorithms {

  def main(args: Array[String]) {

    //initialize list of 10000 sorted and unsorted integers
    val random = Seq.fill(2500)(Random.nextInt).toList
    val sortedRandom = random.sorted

    //test unsorted, pivot at head
    val startTime = System.currentTimeMillis
    val sortedSorted = quicksortHead(sortedRandom);
    val endTime = System.currentTimeMillis
    //test sorted, pivot at head
    val startTime2 = System.currentTimeMillis
    val unsortedSorted = quicksortHead(random);
    val endTime2 = System.currentTimeMillis
    //test sorted, pivot in middle
    val startTime3 = System.currentTimeMillis
    val sortedSorted2 = quicksortMid(sortedRandom);
    val endTime3 = System.currentTimeMillis
    //test unsorted, pivot in middle
    val startTime4 = System.currentTimeMillis
    val unsortedSorted2 = quicksortMid(random);
    val endTime4 = System.currentTimeMillis

    val test = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
    val startTime5 = System.currentTimeMillis
    val sortTest = quicksortMid(test)
    val endTime5 = System.currentTimeMillis

    println("Pivot at head. 2500 sorted integers: " + (endTime-startTime) + " milliseconds.")
    println("Pivot at head. 2500 unsorted integers:" + (endTime2-startTime2) + " milliseconds")
    println("Pivot in middle. 2500 sorted elements: " + (endTime3-startTime3) + " milliseconds.")
    println("Pivot in middle. 2500 unsorted integers: " + (endTime4-startTime4) + " milliseconds.")
    print("The algorithm sorts "); test.map(x => print(x + ", ")); print("to "); sortTest.map(x => print(x + ", "));
  }
}

trait Algorithms {

  def quicksortHead(numbers: List[Int]): List[Int] = {
    if(numbers.length < 2) numbers
    else {
      val pivot = numbers(0)
      quicksortHead(numbers.filter(x => x < pivot)) ++ (numbers.filter(x => x == pivot)) ++ quicksortHead(numbers.filter(x => x > pivot))
    }
  }

  def quicksortMid(numbers: List[Int]): List[Int] = {
    if(numbers.length < 2) numbers
    else {
      val pivot = numbers((numbers.length + 1) / 2)
      quicksortMid(numbers.filter(x => x < pivot)) ++ (numbers.filter(x => x == pivot)) ++ quicksortMid(numbers.filter(x => x > pivot))
    }
  }

}


 /*This is quicksort implemented using functional style. A scala list is an immutable array. The filter method, which is called on the list, returns a new list containing only elements satisfying the predicate given as a parameter. The last line concatenates the left subarrays containing all those elements to the left of the pivot with pivot and the subarray containing all those elements to the right of the pivot. The ++ operator concatenates the lists together. The last line thus results in the sorted array. For quicksortHead, The pivot is chosen to be the first element of the list with each iteration. This was done to demonstrate the worst case scenario where the list is already sorted. If the pivot is at the beginning for a list already sorted, filter would scan every element in the list trying to find a value smaller than the smallest element (at the beginning). If the pivot is in the middle, the work on sorting the list is evenly divided between the quicksort functions.*/

/* Pivot at head. 2500 sorted integers: 152 milliseconds.
   Pivot at head. 2500 unsorted integers:9 milliseconds
   Pivot in middle. 2500 sorted elements: 17 milliseconds.
 Pivot in middle. 2500 unsorted integers: 7 milliseconds.
The algorithm sorts 9, 8, 7, 6, 5, 4, 3, 2, 1, to 1, 2, 3, 4, 5, 6, 7, 8, 9,*/
