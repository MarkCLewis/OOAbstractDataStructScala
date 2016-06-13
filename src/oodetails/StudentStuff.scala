package oodetails

case class Student(name: String, assignments: List[Int], tests: List[Int], quizzes: List[Int])
case class Instructor(name: String, students: List[Student])

object StudentStuff {
  def addGrade(s: Student, test: Int): Student = s.copy(tests = test :: s.tests)

  val participant: AnyRef = Instructor("Lewis", Nil)

  val assignmentAve = participant match {
    case Student(n, a, t, q) =>
      if (a.length == 0) 0.0 else a.sum.toDouble / a.length
    case Instructor(n, ss) =>
      val averages = for (Student(sn, a, t, q) <- ss) yield {
        if (a.length == 0) 0.0 else a.sum.toDouble / a.length
      }
      if (averages.length == 0) 0.0 else averages.sum.toDouble / averages.length
  }

  val assignmentAve2 = participant match {
    case stu: Student =>
      if (stu.assignments.length == 0) 0.0 else stu.assignments.sum.toDouble / stu.assignments.length
    case ins: Instructor =>
      val averages = for (stu: Student <- ins.students) yield {
        if (stu.assignments.length == 0) 0.0 else stu.assignments.sum.toDouble / stu.assignments.length
      }
      if (averages.length == 0) 0.0 else averages.sum.toDouble / averages.length
  }

  val Instructor(iname, Student(sname, a, firstTest :: otherTests, q) :: Nil) = participant

}