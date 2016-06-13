package oodetails.students

class Student(
    val firstName: String,
    val lastName: String,
    private var _quizzes: List[Int] = Nil,
    private var _assignments: List[Int] = Nil,
    private var _tests: List[Int] = Nil) {
  require(_quizzes.forall(q => q >= -20 && q <= 120), "Quiz grades must be in the range of [-20, 120].")
  require(_assignments.forall(a => a >= -20 && a <= 120), "Assignment grades must be in the range of [-20, 120].")
  require(_tests.forall(t => t >= -20 && t <= 120), "Test grades must be in the range of [-20, 120].")

  def quizAverage: Double = if (_quizzes.isEmpty) 0 else _quizzes.sum.toDouble / _quizzes.length

  def assignmentAverage: Double = if (_assignments.isEmpty) 0 else _assignments.sum.toDouble / _assignments.length

  def testAverage: Double = if (_tests.isEmpty) 0 else _tests.sum.toDouble / _tests.length

  def grade: Double = quizAverage * 0.2 + assignmentAverage * 0.5 + testAverage * 0.3

  def addQuiz(newQuiz: Int): Boolean = {
    if (newQuiz >= -20 && newQuiz <= 120) {
      _quizzes ::= newQuiz
      true
    } else false
  }

  def addAssignment(newAssignment: Int): Boolean = {
    if (newAssignment >= -20 && newAssignment <= 120) {
      _assignments ::= newAssignment
      true
    } else false
  }

  def addTest(newTest: Int): Boolean = {
    if (newTest >= -20 && newTest <= 120) {
      _tests ::= newTest
      true
    } else false
  }

  def quizzes = _quizzes

  def assignments = _assignments

  def tests = _tests
}

object Student {
  def apply(firstName: String, lastName: String, quizzes: List[Int] = Nil,
            assignments: List[Int] = Nil, tests: List[Int] = Nil): Student = { 
    new Student(firstName, lastName, quizzes, assignments, tests)
  }
  
  def apply(filename: String): Student = {
    val source = io.Source.fromFile(filename)
    val lines = source.getLines()
    val firstName = lines.next
    val lastName = lines.next
    val quizzes = lines.next.split(" ").map(_.toInt).toList
    val assignments = lines.next.split(" ").map(_.toInt).toList
    val tests = lines.next.split(" ").map(_.toInt).toList
    source.close
    new Student(firstName, lastName, quizzes, assignments, tests)
  }
}