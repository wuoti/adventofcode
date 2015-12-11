import scala.io.Source

val strs = Source.fromFile("input.txt").getLines.toList
  
def isHex(c: Char) = ((0 to 9).toList.map(_.toString).map(_.head) ::: List('a','b','c','d','e','f')).contains(c)
  
def handleAscii(count: Int, s: List[Char]): (List[Char], (Int, Int)) = s match {
  case h :: t if count == 2 && isHex(h) => handleAscii(1, t)
  case h :: t if count == 1 && isHex(h) => (t, (4, 1))
  case h :: t => throw new RuntimeException(s"Could not interpret $h")
  case Nil => throw new RuntimeException("Unexpected end of input")
}
  
def handleEscape(s: List[Char]): (List[Char], (Int, Int)) = s match {
  case '"' :: t => (t, (2, 1))
  case '\\' :: t => (t, (2, 1))
  case 'x' :: t => handleAscii(2, t)
  case h :: t => throw new RuntimeException(s"Could not interpret $h")
  case Nil => throw new RuntimeException("Unexpected end of input")
}
  
  
def handleCharacter(s: List[Char]): (List[Char], (Int, Int)) = s match {
  case '"' :: t => (t, (1, 0))
  case '\\' :: t => handleEscape(t)
  case _ :: t => (t, (1, 1))
  case Nil => throw new RuntimeException("Unexpected end of input")
}

def handleString(prev: (List[Char], (Int, Int))): (Int, Int) = prev match {
  case (Nil, (c, m)) => (c, m)
  case (l, (c, m)) => {
    val (t, (nc, nm)) = handleCharacter(l)
    handleString((t, (c + nc, m + nm)))
  }
}

val (c, m) = strs.foldLeft((0, 0))((c, s) => handleString((s.toList, c)))

println(s"characters in code $c, characters in memory $m, difference ${c - m}")

def encode(s: String) = {
  def encode0(s: List[Char]): String = s match {
    case '"' :: t => """\"""" + encode0(t)
    case '\\' :: t => """\\""" + encode0(t)
    case h :: t => h + encode0(t)
    case Nil => ""
  }
  
  '"' + encode0(s.toList) + '"'
}

println(s"Part 2: ${strs.map(s => encode(s).length - s.length).sum}")