package mapset

import collection.mutable

object SimpleCode extends App {
  def uniqueWords(fileName: String): Set[String] = {
    val source = io.Source.fromFile(fileName)
    val words = source.getLines.toSeq.flatMap(_.split(" +")).
      map(_.filter(_.isLetter).toLowerCase).toSet
    source.close()
    words
  }

  def wordCount(fileName: String): mutable.Map[String, Int] = {
    val source = io.Source.fromFile(fileName)
    val words = source.getLines.toSeq.flatMap(_.split(" +")).
      map(_.filter(_.isLetter).toLowerCase)
    val counts = mutable.Map[String, Int]()
    for (w <- words) {
      if (counts.contains(w)) counts += w -> (counts(w) + 1)
      else counts += w -> 1
    }
    source.close()
    counts
  }

  def wordCount2(fileName: String): Map[String, Int] = {
    val source = io.Source.fromFile(fileName)
    val words = source.getLines.toSeq.flatMap(_.split(" +")).
      map(_.filter(_.isLetter).toLowerCase)
    val counts = words.foldLeft(Map[String, Int]())((m, w) => {
      if (m.contains(w)) m + (w -> (m(w) + 1))
      else m + (w -> 1)
    })
    source.close()
    counts
  }

  def wordCount3(fileName: String): Map[String, Int] = {
    val source = io.Source.fromFile(fileName)
    val counts = source.getLines.toSeq.flatMap(_.split(" +")).
      map(_.filter(_.isLetter).toLowerCase).
      foldLeft(Map[String, Int]())((m, w) =>
        if (m.contains(w)) m + (w -> (m(w) + 1))
        else m + (w -> 1))
    source.close()
    counts
  }

  val nums = Array.tabulate(30)(i => 2*i)
  val everyThird = (nums.indices by 3).map(nums)

}