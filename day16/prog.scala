import scala.io.Source

val input = Source.fromFile("input.txt").getLines

val sues = input.map { l => 
  val sue = l.substring(0, l.indexOf(":"))
  val gifts = l.substring(l.indexOf(":") + 1).split(",")
    .map(_.trim)
    .map(_.split(":")
      .map(_.trim) match {
        case Array(item, count) => (item, count.toInt)
      }
    ).toMap

  (sue -> gifts)
}.toMap

val ticker = Set(
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)
)

val correctSuePart1 = ticker.foldLeft(sues){ case (ss, (item, count)) =>
  ss.filter { case (_, gifts) => 
    val gift = gifts.get(item) 
    gift.isEmpty || gift.contains(count)  
  }
}.head._1

println(s"The correct sue in part 1 is $correctSuePart1")

val correctSuePart2 = ticker.foldLeft(sues){ case (ss, (item, count)) =>
  val comparator = if (Set("cats", "trees").contains(item)) (gc: Int) => gc > count
    else if (Set("pomeranians", "goldfish").contains(item)) (gc: Int) => gc < count
    else (gc: Int) => gc == count

  ss.filter { case (_, gifts) => 
    val gift = gifts.get(item) 
    gift.isEmpty || gift.exists(comparator)
  }
}.head._1

println(s"The correct sue in part 2 is $correctSuePart2")
