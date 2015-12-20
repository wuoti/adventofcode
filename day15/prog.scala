import scala.io.Source

val input = Source.fromFile("input.txt").getLines
val r = """([A-Za-z]+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)
case class Score(score: Int, totalCalories: Int)

val ingredients = input.map {
  case r(name, capacity, durability, flavor, texture, calories) => Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
}.toVector

def calculateScore(recipe: Map[Ingredient, Int]) = Score(
  Math.max(recipe.map{ case (i, a) => a * i.capacity }.sum, 0) * 
  Math.max(recipe.map{ case (i, a) => a * i.durability }.sum, 0) *
  Math.max(recipe.map{ case (i, a) => a * i.flavor }.sum, 0) * 
  Math.max(recipe.map{ case (i, a) => a * i.texture }.sum, 0), 
  Math.max(recipe.map{ case (i, a) => a * i.calories }.sum, 0)
)

val scores = (1 to 100).combinations(ingredients.size)
  .filter(_.sum == 100)
  .flatMap(_.permutations)
  .map(_.zipWithIndex)
  .map(_.map { case (amount, i) => (ingredients(i) -> amount) }.toMap)
  .map(calculateScore)
  .toList

println(s"Max score: ${scores.map(_.score).max}")
println(s"Max score with 500 calories: ${scores.filter(_.totalCalories == 500).map(_.score).max}")