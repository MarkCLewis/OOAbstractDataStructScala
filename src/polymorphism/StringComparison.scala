package polymorphism

object StringComparison extends App {

  class CharInStrComp(val str: String) {
    def positionCompare(str2: String, index: Int): Int = {
      if (index >= str.length) {
        if (index >= str2.length) 0 else -1
      } else if (index >= str2.length) 1 else {
        str(index).compareTo(str2(index))
      }
    }
  }

  class CntCharInStrComp(s: String) extends CharInStrComp(s) {
    var cnt = 0
    override def positionCompare(str2: String, index: Int): Int = {
      cnt += 1
      super.positionCompare(str2, index)
    }
  }

  val csc = new CharInStrComp("Testi")
  println(csc.positionCompare("Testzng", 4))
}