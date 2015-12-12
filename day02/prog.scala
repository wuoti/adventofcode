import scala.io.Source

def extractDimensions(dimStr: String) = dimStr.split('x') match { case Array(l,w,h) => (l.toInt,w.toInt,h.toInt) }

val lines = Source.fromFile("input.txt").getLines().toList.map(extractDimensions)

def calculateWrappingPaper(l: Int, w: Int, h: Int) = {
  val lw = 2 * l * w
  val wh = 2 * w * h
  val hl = 2 * h * l
  
  lw + wh + hl + Math.min(lw, Math.min(wh, hl)) / 2
}

val totalWrappingPaper = lines.foldLeft(0)((acc, l) => acc + (calculateWrappingPaper _).tupled(l))

def calculateRibbon(l: Int, w: Int, h: Int) = Math.min(2 * (l + w), Math.min(2 * (l + h), 2 * (w + h))) + l * w * h

val totalRibbon = lines.foldLeft(0)((acc, l) => acc + (calculateRibbon _).tupled(l))

println(s"Total wrapping paper needed $totalWrappingPaper square feet")
println(s"Total ribbon needed $totalRibbon feet")