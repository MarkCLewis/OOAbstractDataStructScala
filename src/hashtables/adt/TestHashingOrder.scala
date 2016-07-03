package hashtables.adt

object TestHashingOrder extends App {
  val chm = ChainingHashMap[Double, Double](HashingMethods.Division)
  for (i <- 1 to 1000000) chm += math.random -> math.random
  println(chm.maxBin)
}