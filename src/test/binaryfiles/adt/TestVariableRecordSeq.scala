package test.binaryfiles.adt

import java.io.DataInput
import java.io.DataOutput
import java.io.File

import org.junit.Assert._
import org.junit.Before
import org.junit.Test

import binaryfiles.adt.VariableRecordSeq

class TestVariableRecordSeq {
  private var iFile: File = null
  private var dFile: File = null
  private case class PointRec(x: Double, y: Double, z: Double, d: Int)
  private case class StudentRec(first: String, last: String, grades: List[Int])

  @Before def setFile: Unit = {
    iFile = new File("TextFile.ind")
    iFile.deleteOnExit()
    if (iFile.exists()) iFile.delete()
    dFile = new File("TextFile.dat")
    dFile.deleteOnExit()
    if (dFile.exists()) dFile.delete()
  }

  @Test def emptyStart: Unit = {
    val frs = new VariableRecordSeq[Int](iFile, dFile, din => din.readInt(), (dout, i) => dout.writeInt(i))
    assertTrue(frs.length == 0)
  }

  @Test def write4: Unit = {
    val frs = new VariableRecordSeq[Int](iFile, dFile, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frs(0) = 3
    frs(1) = 8
    frs(2) = 2
    frs(3) = 9
    assertEquals(4, frs.length)
    assertEquals(3, frs(0))
    assertEquals(8, frs(1))
    assertEquals(2, frs(2))
    assertEquals(9, frs(3))
  }

  @Test def write100: Unit = {
    val frs = new VariableRecordSeq[Double](iFile, dFile, din => din.readDouble(), (dout, d) => dout.writeDouble(d))
    val nums = Array.fill(100)(math.random)
    for (i <- nums.indices) frs(i) = nums(i)
    assertEquals(nums.length, frs.length)
    for (i <- nums.indices) assertEquals(nums(i), frs(i), 0)
  }

  private def readPoint(din: DataInput): PointRec = {
    new PointRec(din.readDouble(), din.readDouble(), din.readDouble(), din.readInt())
  }

  private def writePoint(dout: DataOutput, p: PointRec): Unit = {
    dout.writeDouble(p.x)
    dout.writeDouble(p.y)
    dout.writeDouble(p.z)
    dout.writeInt(p.d)
  }

  @Test def writePoints: Unit = {
    val frs = new VariableRecordSeq[PointRec](iFile, dFile, readPoint, writePoint)
    val pnts = Array.fill(10)(new PointRec(math.random, math.random, math.random, util.Random.nextInt))
    for (i <- pnts.indices) frs(i) = pnts(i)
    assertEquals(pnts.length, frs.length)
    for (i <- pnts.indices) assertEquals(pnts(i), frs(i))
  }

  @Test def rewritePoints: Unit = {
    val frs = new VariableRecordSeq[PointRec](iFile, dFile, readPoint, writePoint)
    val pnts = Array.fill(10)(new PointRec(math.random, math.random, math.random, util.Random.nextInt))
    for (i <- pnts.indices) frs(i) = pnts(i)
    assertEquals(pnts.length, frs.length)
    for (i <- pnts.indices) assertEquals(pnts(i), frs(i))
    for (i <- 0 until pnts.length / 2) {
      val index = util.Random.nextInt(pnts.length)
      pnts(index) = new PointRec(math.random, math.random, math.random, util.Random.nextInt)
      frs(index) = pnts(index)
    }
    for (i <- pnts.indices) assertEquals(pnts(i), frs(i))
  }

  private def readStudent(din: DataInput): StudentRec = {
    val first = din.readUTF()
    val last = din.readUTF()
    val num = din.readInt()
    val grades = List.fill(num)(din.readInt())
    new StudentRec(first, last, grades)
  }

  private def writeStudent(dout: DataOutput, s: StudentRec): Unit = {
    dout.writeUTF(s.first)
    dout.writeUTF(s.last)
    dout.writeInt(s.grades.length)
    s.grades.foreach(dout.writeInt)
  }

  @Test def writeStudents: Unit = {
    val frs = new VariableRecordSeq[StudentRec](iFile, dFile, readStudent, writeStudent)
    val students = Array.fill(100) {
      val first = Array.fill(util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val last = Array.fill(util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
      new StudentRec(first, last, grades)
    }
    for (i <- students.indices) frs(i) = students(i)
    assertEquals(students.length, frs.length)
    for (i <- students.indices) assertEquals(students(i), frs(i))
  }

  @Test def rewriteStudents: Unit = {
    val frs = new VariableRecordSeq[StudentRec](iFile, dFile, readStudent, writeStudent)
    val students = Array.fill(100) {
      val first = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val last = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
      new StudentRec(first, last, grades)
    }
    for (i <- students.indices) frs(i) = students(i)
    assertEquals(students.length, frs.length)
    for (i <- students.indices) assertEquals(students(i), frs(i))
    for (i <- 0 until students.length / 2) {
      val index = util.Random.nextInt(students.length)
      students(index) = {
        val first = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
        val last = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
        val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
        new StudentRec(first, last, grades)
      }
      frs(index) = students(index)
    }
    for (i <- students.indices) assertEquals(students(i), frs(i))
  }
}