import scala.io.Source
val words = Source.fromFile("input.txt").getLines.toList

def countVowels(word: String) = "aeiou".foldLeft(0)((acc, v) => acc + word.count(_ == v))
def containsOneLetterTwice(word: String) = word.sliding(2, 1).exists(l => l(0) == l(1))
def containsNaughtyBits(word: String) = Set("ab", "cd", "pq", "xy").exists(nb => word.contains(nb))

def isNice(word: String) = countVowels(word) >= 3 && containsOneLetterTwice(word) && !containsNaughtyBits(word)

println(s"Total number of nice words with first criteria: ${words.count(isNice)}")

def foo(word: String): Boolean = if (word.isEmpty) false else word.drop(2).contains(word.take(2)) || foo(word.drop(1))
def bar(word: String) = word.sliding(3, 1).map(_.toList).exists{ case List(c1, _, c2) => c1 == c2 }

def isNice2(word: String) = foo(word) && bar(word)

println(s"Total number of nice words with second criteria: ${words.count(isNice2)}")
