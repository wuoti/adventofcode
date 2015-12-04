import scala.io.Source

val pars = Source.fromFile("input.txt").toList

def upOrDown(c: Char) = if (c == '(') 1 else -1

val finalFloor = pars.foldLeft(0)((floor, p) => floor + upOrDown(p))

def foo(floor: Int, seq: Int, pars: List[Char]): Int = {
  if (floor == -1) seq 
  else pars match {
    case h :: t => foo(floor + upOrDown(h), seq + 1, t)
    case Nil => sys.error("Never reached basement")
  }    
}

val firstTimeInBasement = foo(0, 0, pars)

println(s"The instructions take santa to floor $finalFloor")
println(s"The first time santa enters the basement is at instruction number $firstTimeInBasement")
