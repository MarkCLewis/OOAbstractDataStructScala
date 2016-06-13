package oobasics

object GradeBook {
  def main(args: Array[String]): Unit = {
    val student = new Student("Jane", "Doe", List(97, 80), List(100), List(89))
    printGrade(student)
  }
  
  def printGrade(s:Student):Unit = {
    println(s.firstName+" "+s.lastName)
    println(s.grade)
  }
}