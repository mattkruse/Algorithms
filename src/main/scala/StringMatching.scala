package matching

import scala.io.Source
import org.apache.commons.lang.StringUtils._


object Horspools {

  def matching(pattern: String, text: String): Int = {
    val allCharacters = pattern + text
    val startTime0 = System.currentTimeMillis
    val shiftTable = generateShiftTable(pattern, allCharacters)
    val endTime1 = System.currentTimeMillis
    println("The shift table was generated in " + (endTime1 - startTime0) + " milliseconds.")
    var index = pattern.length - 1
    var comparisons = 0

    while(index <= text.length - 1) {
      var matchedCharacters = 0
      while(matchedCharacters <= pattern.length - 1 && pattern(pattern.length - 1 - matchedCharacters) == text(index - matchedCharacters)) {
        comparisons += 1
        matchedCharacters = matchedCharacters + 1
      }
      if (matchedCharacters == pattern.length) {
        println("Horspools had " + comparisons + " comparisons.")
        return index - pattern.length + 1
      } else {
        comparisons += 1
        index = index + shiftTable(text(index))
      }
    }
    println("Horspools had " + comparisons + " comparisons.")
    return -1
  }

  def generateShiftTable(pattern: String, allCharacters: String): Map[Char, Int] = {
    var table = allCharacters.foldLeft(scala.collection.mutable.Map[Char, Int]())((table, character) => table + (character -> pattern.length))
    for (x <- 0 to pattern.length - 2) {
      table += (pattern(x) -> (pattern.length - 1 - x))
    }
    table.toMap
  }



}

object BruteForce {

  def matching(pattern: String, text: String): Int = {
    var index = pattern.length - 1
    var comparisons = 0

    while(index <= text.length - 1) {
      var matchedCharacters = 0
      while(matchedCharacters <= pattern.length - 1 && pattern(pattern.length - 1 - matchedCharacters) == text(index - matchedCharacters)) {
        matchedCharacters = matchedCharacters + 1
        comparisons += 1
      }
      if (matchedCharacters == pattern.length) {
        println("BruteForce had " + comparisons + " comparisons.")
        return index - pattern.length + 1
      } else {
        comparisons += 1
        index = index + 1
      }
    }
    println("BruteForce had " + comparisons + " comparisons.")

    return -1
  }

}

object Benchmark {

  def main(args: Array[String]) {

    val testData = Source.fromFile("./src/main/resources/text").mkString
    val pattern = "AATTGACATAGACCCAAG-------------------------------------------------------------TCATCTAGTAAAATGAGGATG-----AT-------------GCGTCGGGAATG"

    val startTime1 = System.currentTimeMillis
    val horspoolResult = Horspools.matching(pattern, testData)
    val endTime1 = System.currentTimeMillis

    val startTime2 = System.currentTimeMillis
    val bruteForceResult = BruteForce.matching(pattern, testData)
    val endTime2 = System.currentTimeMillis

    println("The text has " + testData.length + " characters.")
    println("Horspools found the pattern " + "\"" + pattern + "\"" + " at index " + horspoolResult)
    println("BruteForce found the pattern " +  "\"" + pattern + "\"" + " at index " + bruteForceResult)
    println("The Horspools algorithm took " + (endTime1 - startTime1) + " milliseconds.")
    println("The BruteForce algorithm took " + (endTime2 - startTime2) + " milliseconds.")

  }

}
