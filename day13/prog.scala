import scala.io.Source

val input = Source.fromFile("input.txt").getLines

val r = """([A-Za-z]+) would (gain|lose) (\d+) happiness units by sitting next to ([A-Za-z]+).""".r

val hus = input.foldLeft(Map[String, Map[String, Int]]()) {
  case (acc, r(n1, plusMinus, hu, n2)) => acc.updated(n1, acc.get(n1).getOrElse(Map[String, Int]()).updated(n2, (if (plusMinus == "gain") 1 else -1) * hu.toInt))
}

def calculateHappiness(hus: Map[String, Map[String, Int]], so: Vector[String]) = (so.last +: so :+ so.head).sliding(3, 1).foldLeft(0) {
  case (acc, Vector(s1, p, s2)) => acc + hus(p)(s1) + hus(p)(s2)
}

println(s"Total change in happiness for the optimal seating arrangement in part 1: ${hus.keys.toVector.permutations.foldLeft(Int.MinValue) {
  case (prev, so) => Math.max(prev, calculateHappiness(hus, so))
}}")

val me = "me"
val hus2 = hus.map {
  case (k, v) => (k -> v.updated(me, 0))
}.updated(me, hus.keys.map(k => (k -> 0)).toMap)

println(s"Total change in happiness for the optimal seating arrangement in part 2: ${hus2.keys.toVector.permutations.foldLeft(Int.MinValue) {
  case (prev, so) => Math.max(prev, calculateHappiness(hus2, so))
}}")