val input = "1321131112".map(_.toString.toInt).toVector

def foo(c: Option[(Int, Int)], s: Vector[Int], acc: Vector[Int]): Vector[Int] = s match {
  case h +: t if c.isEmpty => foo(Some((h, 1)), t, acc)
  case h +: t => if (c.map(_._1).contains(h)) foo(c.map{ case (ch, c) => (ch, c + 1) }, t, acc) else foo(Some((h, 1)), t, acc :+ c.map(_._2).get :+ c.map(_._1).get)
  case _ if c.isEmpty => acc
  case _ => acc :+ c.map(_._2).get :+ c.map(_._1).get
}

println(s"Part 1: ${(1 to 50).foldLeft(input) { case (prev, i) => {
  if (i == 41) println(s"Part 1: ${prev.size}")
  foo(None, prev, Vector())
}}.size}")