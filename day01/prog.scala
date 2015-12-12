import scala.io.Source

val pars = Source.fromFile("input.txt").toStream

def upOrDown(c: Char) = if (c == '(') 1 else -1

val finalFloor = pars.foldLeft(0)((floor, p) => floor + upOrDown(p))

val firstTimeInBasement = pars
  .map(upOrDown)
  .scanLeft(0){ case (floor, d) => floor + d }
  .zipWithIndex
  .filter(_._1 == -1)
  .head._2

println(s"The instructions take santa to floor $finalFloor")
println(s"The first time santa enters the basement is at instruction number $firstTimeInBasement")
