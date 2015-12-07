import scala.io.Source
import scala.util.Try

val instrs = Source.fromFile("input.txt").getLines

val signalR = """([a-z||\d]+) -> ([a-z|\d]+)""".r
val andR = """([a-z|\d]+) AND ([a-z|\d]+) -> ([a-z]+)""".r
val orR = """([a-z|\d]+) OR ([a-z|\d]+) -> ([a-z]+)""".r
val lshiftR = """([a-z|\d]+) LSHIFT (\d+) -> ([a-z]+)""".r
val rshiftR = """([a-z|\d]+) RSHIFT (\d+) -> ([a-z]+)""".r
val notR = """NOT ([a-z|\d]+) -> ([a-z]+)""".r

trait Signal

case class And(s1: Signal, s2: Signal) extends Signal
case class Or(s1: Signal, s2: Signal) extends Signal
case class LShift(s: Signal, shift: Int) extends Signal
case class RShift(s1: Signal, shift: Int) extends Signal
case class Not(s: Signal) extends Signal
case class BitSignal(value: Int) extends Signal
case class WireSignal(id: String) extends Signal

def foo(s: String) = Try(s.toInt).map(BitSignal).getOrElse(WireSignal(s))

val wirings = instrs.foldLeft(Map[String, Signal]())((acc, i) => {
  acc + (i match {
    case andR(s1, s2, o) => (o -> And(foo(s1), foo(s2)))
    case orR(s1, s2, o) => (o -> Or(foo(s1), foo(s2)))
    case lshiftR(s, sf, o) => (o -> LShift(foo(s), sf.toInt))
    case rshiftR(s, sf, o) => (o -> RShift(foo(s), sf.toInt))
    case notR(s, o) => (o -> Not(foo(s)))
    case signalR(s, o) => (o -> foo(s))
  })
})

val cache = scala.collection.mutable.Map[Signal, Int]()

def calculateSignal(wires: Map[String, Signal], signal: Signal): Int = {
  cache.getOrElseUpdate(signal, signal match {
    case And(s1, s2) => calculateSignal(wires, s1) & calculateSignal(wires, s2)
    case Or(s1, s2) => calculateSignal(wires, s1) | calculateSignal(wires, s2)
    case LShift(s, sf) => calculateSignal(wires, s) << sf
    case RShift(s, sf) => calculateSignal(wires, s) >> sf
    case Not(s) => 65535 - calculateSignal(wires, s)
    case WireSignal(s) => calculateSignal(wires, wires(s))
    case BitSignal(s) => s
  })
}

val part1Signal = calculateSignal(wirings, wirings("a"))
cache.clear
val part2Signal = calculateSignal(wirings.updated("b", BitSignal(part1Signal)), wirings("a"))

println(s"Part 1 signal for A: $part1Signal")
println(s"Part 2 signal for A: $part2Signal")