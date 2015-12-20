import scala.io.Source

val input = Source.fromFile("input.txt").getLines

object State extends Enumeration {
  type State = Value
  val Flying, Resting = Value
}

import State._

case class Reindeer(name: String, speed: Int, endurance: Int, restTime: Int)

val r = """([A-Za-z]+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r
val reindeers = input.map {
  case r(name, speed, endurance, restTime) => Reindeer(name, speed.toInt, endurance.toInt, restTime.toInt)
}.toList

val situation = ((1 to 2503).foldLeft(reindeers.map(r => r -> (State.Flying, 0, 0, 0)).toMap) { case (rs, i) =>
  val r = rs.map {
    case (r @ Reindeer(_, speed, endurance, restTime), (state, stateDuration, dst, stars)) => 
      val newDst = if (state == State.Flying) dst + speed else dst
      val (newState, newStateDuration) = if (state == State.Flying && (stateDuration + 1) == endurance) (State.Resting, 0) else if (state == State.Resting && (stateDuration + 1) == restTime) (State.Flying, 0) else (state, stateDuration + 1)
      (r, (newState, newStateDuration, newDst, stars))
  }.toMap


  val highestDst = r.values.map { case (_, _, dst, _) => dst }.max
  
  r.mapValues { case (state, sd, dst, stars) => (state, sd, dst, if (dst == highestDst) stars + 1 else stars) }
})


println(s"Part 1: ${situation.values.map(v => v._3).max}")
println(s"Part 2: ${situation.values.map(v => v._4).max}")