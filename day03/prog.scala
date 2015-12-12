import scala.io.Source

val directions = Source.fromFile("input.txt").toList

def foo(acc: Map[(Int, Int), Int], position: (Int, Int), ds: List[Char]): Map[(Int, Int), Int] = {
  val updated = acc.updated(position, acc.get(position).map(_ + 1).getOrElse(1))  
  ds match {
    case h :: t => 
      val (curX, curY) = position
      val pos: (Int, Int) = if (h == '^') (curX, curY + 1)
                else if (h == 'v') (curX, curY - 1)
                else if (h == '<') (curX - 1, curY)
                else (curX + 1, curY)
                
      foo(updated, pos, t)
    case Nil => updated
  }
}

// 1 
val housesWithPresents = foo(Map[(Int,Int),Int](), (0,0), directions)

val (santaDirections, roboDirections) = directions.zipWithIndex.partition(_._2 % 2 == 0) match { case (l1, l2) => (l1.map(_._1), l2.map(_._1)) }

val santaDelivers = foo(Map[(Int,Int),Int](), (0,0), santaDirections)
val roboDelivers = foo(Map[(Int,Int),Int](), (0,0), roboDirections)

// 2
val combinedDeliveries = (santaDelivers.toList ++ roboDelivers.toList).groupBy(_._1).map{ case (k, lst) => (k, lst.map(_._2).sum)}

println(s"Only Santa delivering presents. Number of houses that get at least one present: ${housesWithPresents.values.count(_ >= 1)}")
println(s"Santa and Robo Santa delivering presents. Number of houses that get at least one present: ${combinedDeliveries.values.count(_ >= 1)}")