package implementations

import scala.collection.mutable.Map

object DamerauLevenshteinDistance {

  def apply(source: String, target: String): Int = {

    val sourceLength = source.length
    val targetLength = target.length
    val allCharacters = source + target

    val sourceIndexByCharacter: Map[Char, Int] = allCharacters.foldLeft(Map[Char, Int]())((table, character) => table + (character -> 0))

    val table = Array.ofDim[Int](sourceLength, targetLength)


    for (i <- 0 to sourceLength - 1) {
      var maxSourceLetterMatchIndex = 0
        for (j <- 0 to targetLength - 1) {

          table(i)(j) = if (i == 0) j else if (j == 0) i else 0

          if (i > 0 && j > 0) {
            val candidateSwapIndex = sourceIndexByCharacter(target(j))
            val jSwap = maxSourceLetterMatchIndex
            val iSwap = candidateSwapIndex

            if (source(i) == target(j)) maxSourceLetterMatchIndex = j

            var preSwapCost = 0
            var swapDistance = 999
            val matchCost = if(source(i) == target(j)) 0 else 1

            if (iSwap != 0 && jSwap != 0) {
               preSwapCost = table(max(0, iSwap - 1))(max(0, jSwap - 1))
               swapDistance = preSwapCost + (i - iSwap - 1) + (j - jSwap - 1) + 1
            }

            table(i)(j) = min(table(i-1)(j) + 1, table(i)(j-1) + 1, table(i-1)(j-1) + matchCost, swapDistance)

          }
        }
      sourceIndexByCharacter += (source(i) -> i)
    }

    /*for (x <- 0 to sourceLength - 1 ) {
      for (y <- 0 to targetLength -1) {
        print(table(x)(y)+" ")

      }
      println("\n")
    }*/

    return table(sourceLength - 1)(targetLength - 1)

  }

  private def min(ns: Int*) = ns.reduceLeft(math.min)

  private def max(ns: Int*) = ns.reduceLeft(math.max)

}
