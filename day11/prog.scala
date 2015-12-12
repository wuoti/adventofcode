
val alphabet = "abcdefghijklmnopqrstuvwxyz"
val seqsOfThree = alphabet.toList.sliding(3, 1).toSet

def bar(str: Vector[Char]): Boolean = str.take(2) match {
  case Vector(c1, c2) if c1 == c2 => true
  case Vector(_ , _) => bar(str.drop(1))
  case _ => false
}

def foo(str: Vector[Char]): Boolean = str.take(2) match {
  case Vector(c1, c2) if c1 == c2 => bar(str.drop(2))
  case Vector(_, _) => foo(str.drop(1))
  case _ => false
}

def isValid(password: Vector[Char]) = password.sliding(3, 1).exists(s => seqsOfThree.contains(s.toList)) && foo(password) && !Set('i', 'o', 'l').exists(c => password.contains(c))

def roll(c: Char) = if (c == 'z') 'a' else alphabet(alphabet.indexOf(c) + 1)

def findNextValid(head: Vector[Char], tail: Vector[Char]): Vector[Char] = {
  if (head.isEmpty) findNextValid(head ++ tail, Vector())
  else {
    val newHead = head.dropRight(1) :+ roll(head.last)
    if (isValid(newHead ++ tail)) newHead ++ tail
    else {
      if (newHead.last == 'a') findNextValid(newHead.dropRight(1), newHead.last +: tail)
      else findNextValid(newHead ++ tail, Vector())
    }
  }
}

val input = "hxbxwxba"
val nextValid1 = findNextValid(input.toVector, Vector()).mkString("")
val nextValid2 = findNextValid(nextValid1.toVector, Vector()).mkString("")

println(s"The next valid password is $nextValid1")
println(s"And the next valid password after this one is $nextValid2")