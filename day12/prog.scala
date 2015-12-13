import scala.io.Source

val input = Source.fromFile("input.txt").getLines.next

val number = """(-?\d+)""".r

println(s"Sum of all numbers in part 1: ${number.findAllIn(input).map(_.toInt).sum}")

trait JValue
case class JString(s: String) extends JValue {
  override def toString = '"' + s + '"'
}
case class JInt(i: Int) extends JValue {
  override def toString = i.toString
}
case class JArray(values: List[JValue]) extends JValue {
  override def toString = s"[${values.map(_.toString).mkString(",")}]"
}
case class JField(key: JString, value: JValue) {
  override def toString = s"${key.toString}:${value.toString}"
}
case class JObject(fields: Set[JField]) extends JValue {
  override def toString = s"{${fields.map(_.toString).mkString(",")}}"
}

def expect(e: Char, c: Char) = if (e != c) throw new RuntimeException(s"Unexpedted $c")

def expectMultiple(e: Set[Char => Boolean], c: Char) = if (!e.exists(_(c))) throw new RuntimeException(s"Unexpected $c")

def parseJString(s: List[Char]): (JString, List[Char]) = {
  expect('"', s.head)
  val ss = s.tail.takeWhile(_ != '"')
  expect('"', s.tail(ss.length))
  (JString(ss.mkString("")), s.drop(ss.length + 2))
}

def parseJInt(s: List[Char]): (JInt, List[Char]) = {
  expectMultiple(Set((c: Char) => c == '-', (c: Char) => c.isDigit), s.head)
  val (isNeg, ss) = if (s.head == '-') (true, s.tail) else (false, s)
  
  val nstr = ss.takeWhile(_.isDigit)
  (JInt((if (isNeg) -1 else 1) * nstr.mkString("").toInt), ss.drop(nstr.length))
}

def values(s: List[Char], acc: Vector[JValue]): (List[JValue], List[Char]) = {
  val (v, ss) = parseJValue(s)
  ss match {
    case ',' :: t => values(t, acc :+ v)
    case ']' :: t => ((acc :+ v).toList, ss)
    case h :: _ => throw new RuntimeException(s"Unexpected $h")
    case Nil => throw new RuntimeException("Unexpected end of input")
  }
}

def parseJArray(s: List[Char]): (JArray, List[Char]) = {
  expect('[', s.head)  
  val (vs, ss) = values(s.tail, Vector())
  expect(']', ss.head)
  (JArray(vs), ss.tail)
}

def parseJField(s: List[Char]): (JField, List[Char]) = {
  val (key, ss) = parseJString(s)
  expect(':',ss.head)
  val (value, ss2) = parseJValue(ss.tail)
  (JField(key, value), ss2)
}

def fields(s: List[Char], acc: Set[JField]): (Set[JField], List[Char]) = {
  val (f, ss) = parseJField(s)
  ss match {
    case ',' :: t => fields(t, acc + f)
    case '}' :: t => (acc + f, ss)
    case h :: _ => throw new RuntimeException(s"Unexpected $h")
    case Nil => throw new RuntimeException("Unexpected end of input")
  }
}

def parseJObject(s: List[Char]): (JObject, List[Char]) = {
  expect('{', s.head)  
  val (fs, ss) = fields(s.tail, Set())
  expect('}', ss.head)
  (JObject(fs), ss.tail)
}

def parseJValue(s: List[Char]): (JValue, List[Char]) = s match {
  case '{' :: t => parseJObject(s)
  case '"' :: t => parseJString(s)
  case '[' :: t => parseJArray(s)
  case h :: t if h.isDigit || h == '-' => parseJInt(s)
  case h :: _ => throw new RuntimeException(s"Unexpected $h")
  case Nil => throw new RuntimeException("Unexpected end of input")
}

val json = parseJValue(input.toList)._1

def containsRed(fs: Set[JField]) = fs.exists {
  case JField(_, JString(s)) if s == "red" => true
  case _ => false
}

def collectInts(jv: JValue): List[Int] = jv match {
  case JObject(fs) if containsRed(fs) => List()
  case JObject(fs) => fs.map{ case JField(_, v) => v }.map(f => collectInts(f)).toList.flatten
  case JArray(values) => values.map(v => collectInts(v)).flatten
  case JString(_) => List()
  case JInt(i) => List(i)
}

println(s"Sum of all numbers in part 2: ${collectInts(json).sum}")
