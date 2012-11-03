package matching


object Horspools {

  def matching(pattern: String, text: String): Int = {
    val allCharacters = pattern + text
    val shiftTable = generateShiftTable(pattern, allCharacters)
    var index = pattern.length - 1

    while(index <= text.length - 1) {
      var matchedCharacters = 0
      while(matchedCharacters <= pattern.length - 1 && pattern(pattern.length - 1 - matchedCharacters) == text(index - matchedCharacters)) {
        matchedCharacters = matchedCharacters + 1
        println("match")
      }
      if (matchedCharacters == pattern.length) {
        return index - pattern.length + 1
      } else {
        index = index + shiftTable(text(index))
        println("no match")
      }
    }
    return -1
  }

  def generateShiftTable(pattern: String, allCharacters: String): Map[Char, Int] = {
    var table = allCharacters.foldLeft(scala.collection.mutable.Map[Char, Int]())((table, character) => table + (character -> pattern.length))
    for (x <- 0 to pattern.length - 2) {
      table += (pattern(x) -> (pattern.length - 1 - x))
      println(pattern.length - 1 - x)
    }
    table.toMap
  }

}

object BruteForce {

  def matching(pattern: String, text: String): Int = {
    var index = pattern.length - 1

    while(index <= text.length - 1) {
      var matchedCharacters = 0
      while(matchedCharacters <= pattern.length - 1 && pattern(pattern.length - 1 - matchedCharacters) == text(index - matchedCharacters)) {
        matchedCharacters = matchedCharacters + 1
        println("match")
      }
      if (matchedCharacters == pattern.length) {
        return index - pattern.length + 1
      } else {
        index = index + 1
        println("no match")
      }
    }
    return -1
  }

}
