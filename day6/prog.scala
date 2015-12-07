import scala.io.Source
val cmds = Source.fromFile("input.txt").getLines

val lights: Seq[collection.mutable.Seq[Boolean]] = (0 to 999).map(_ => (0 to 999).map(_ => false).to[collection.mutable.Seq])
val lights2: Seq[collection.mutable.Seq[Int]] = (0 to 999).map(_ => (0 to 999).map(_ => 0).to[collection.mutable.Seq])

val toggleCmd = "toggle"
val turnOnCmd = "turn on"
val turnOffCmd = "turn off"

def switchLights(ls: Seq[collection.mutable.Seq[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int, cmd: String): Unit = for { 
  x <- x1 to x2
  y <- y1 to y2 
} ls(x)(y) = if (cmd == toggleCmd) !ls(x)(y) else if (cmd == turnOnCmd) true else false

def setBrightness(ls: Seq[collection.mutable.Seq[Int]], x1: Int, y1: Int, x2: Int, y2: Int, cmd: String): Unit = for { 
  x <- x1 to x2
  y <- y1 to y2 
} ls(x)(y) = if (cmd == toggleCmd) ls(x)(y) + 2 else if (cmd == turnOnCmd) ls(x)(y) + 1 else Math.max(0, ls(x)(y) - 1)

cmds.foreach {c => 
  val cmd = c.takeWhile(!_.isDigit).dropRight(1)
  val (x1, y1, x2, y2) = c.stripPrefix(cmd + " ").split(' ') match {
    case Array(coord1, _, coord2) => {
      val p = (s: String) => s.split(',') match { case Array(x, y) => (x.toInt, y.toInt) }
      val (x1, y1) = p(coord1)
      val (x2, y2) = p(coord2)
      (x1, y1, x2, y2)
    }
  }
  
  switchLights(lights, x1, y1, x2, y2, cmd)
  setBrightness(lights2, x1, y1, x2, y2, cmd)
}

println(s"Number of lights turned on: ${lights.flatten.count(_ == true)}")
println(s"Total brightness of all lights combined: ${lights2.flatten.sum}")