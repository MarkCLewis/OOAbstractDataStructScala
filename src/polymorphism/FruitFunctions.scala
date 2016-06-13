package polymorphism

// Required to make things compile
class Skin

abstract class Fruit {
  def canEatSkin: Boolean
  def fractionalLiquidContent: Double
  def peel(): Skin
}

class Cherry extends Fruit {
  def canEatSkin: Boolean = true
  def fractionalLiquidContent: Double = 0.2
  def peel(): Skin = new Skin
}

// Required to make things compile
class Blender {
  def +=(o: Any): Unit = ???
  def blend(): Unit = ???
}

object FruitFunctions {
  val blender = new Blender
  val juice = "Orange juice"
  val ice = "Ice"

  def makeBreakfastShake(fruit: Fruit): Unit = {
    if (!fruit.canEatSkin) {
      fruit.peel()
    }
    blender += fruit
    blender += juice
    if (fruit.fractionalLiquidContent < 0.3) blender += juice
    blender += ice
    blender.blend
  }

  def makeBreakfastShake(fruits: Array[Fruit]) {
    for (fruit <- fruits) {
      if (!fruit.canEatSkin) {
        fruit.peel
      }
      blender += fruit
    }
    blender += juice
    blender += ice
    blender.blend
  }

  def subCherries(bowl: Array[Fruit]) {
    if (!bowl.isEmpty) bowl(0) = new Cherry
  }

  def makeBreakfastShake2[A <: Fruit](fruits: Array[A]) {
    for (fruit <- fruits) {
      if (!fruit.canEatSkin) {
        fruit.peel
      }
      blender += fruit
    }
    blender += juice
    blender += ice
    blender.blend
  }

  def makeBreakfastShake(fruits: List[Fruit]) {
    for (fruit <- fruits) {
      if (!fruit.canEatSkin) {
        fruit.peel
      }
      blender += fruit
    }
    blender += juice
    blender += ice
    blender.blend
  }

}