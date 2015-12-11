import scala.io.Source
import scala.util.Try

val dsts = Source.fromFile("input.txt").getLines

val dstR = """([A-Za-z\d]+) to ([A-Za-z\d]+) = (\d+)""".r
case class Route(p1: String, p2: String, distance: Int)

def updateRoutes(m: Map[String, Map[String, Int]], from: String, to: String, dst: Int) = m.updated(from, m.get(from).getOrElse(Map[String, Int]()).updated(to, dst))

val routes = dsts.map {
  case dstR(p1, p2, dst) => Route(p1, p2, dst.toInt)
}.foldLeft(Map[String, Map[String, Int]]()){
   case (acc, Route(p1, p2, dst)) => updateRoutes(updateRoutes(acc, p1, p2, dst), p2, p1, dst)
}

def findRoute(place: String, visited: List[String], dst: Int, comparator: (Int, Int) => Boolean): (List[String], Int) = {
  routes(place).filter { case (k, _) => !visited.contains(k) && k != place } match {
    case s if s.isEmpty => (place :: visited, dst)
    case s => s.map { case (p, ds) => findRoute(p, place :: visited, dst + ds, comparator)}.reduceLeft((a, b) => if (comparator(a._2, b._2)) a else b)
  }
}

def findLongestOrShortestRoute(shortest: Boolean = true) = {
  val shortestComparator = (a: Int, b:Int) => a < b
  val comparator = if (shortest) shortestComparator else (a: Int, b: Int) => !shortestComparator(a, b)
  
  routes.keys.map(p => findRoute(p, List(), 0, comparator)).reduceLeft((a, b) => if (comparator(a._2, b._2)) a else b)
}

def findShortestRoute() = findLongestOrShortestRoute(true)
def findLongestRoute() = findLongestOrShortestRoute(false)

val shortest = findShortestRoute()
val longest = findLongestRoute()

println(s"The shortest route is ${shortest._2}: ${shortest._1.mkString(", ")}")
println(s"The longest route is ${longest._2}: ${longest._1.mkString(", ")}")