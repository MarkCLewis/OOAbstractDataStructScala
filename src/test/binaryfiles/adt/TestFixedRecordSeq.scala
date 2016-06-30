package test.binaryfiles.adt

import java.io.DataInput
import java.io.DataOutput
import java.io.File

import org.junit.Assert._
import org.junit.Before
import org.junit.Test

import binaryfiles.adt.FixedRecordSeq

class TestFixedRecordSeq {
  private var file: File = null
  private case class PointRec(x: Double, y: Double, z: Double, d: Int)
  private case class StudentRec(first: String, last: String, grades: List[Int])

  @Before def setFile: Unit = {
    file = new File("TextFile.bin")
    file.deleteOnExit()
    if (file.exists()) file.delete()
  }

  @Test def emptyStart: Unit = {
    val frs = new FixedRecordSeq[Int](file, 4, din => din.readInt(), (dout, i) => dout.writeInt(i))
    assertTrue(frs.length == 0)
  }

  @Test def write4: Unit = {
    val frs = new FixedRecordSeq[Int](file, 4, din => din.readInt(), (dout, i) => dout.writeInt(i))
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
    val frs = new FixedRecordSeq[Double](file, 8, din => din.readDouble(), (dout, d) => dout.writeDouble(d))
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
    val frs = new FixedRecordSeq[PointRec](file, 3 * 8 + 4, readPoint, writePoint)
    val pnts = Array.fill(10)(new PointRec(math.random, math.random, math.random, util.Random.nextInt))
    for (i <- pnts.indices) frs(i) = pnts(i)
    assertEquals(pnts.length, frs.length)
    for (i <- pnts.indices) assertEquals(pnts(i), frs(i))
  }

  @Test def rewritePoints: Unit = {
    val frs = new FixedRecordSeq[PointRec](file, 3 * 8 + 4, readPoint, writePoint)
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
    val buf = new Array[Byte](20)
    din.readFully(buf)
    val first = new String(buf.takeWhile(_ > 0))
    din.readFully(buf)
    val last = new String(buf.takeWhile(_ > 0))
    val grades = List.fill(10)(din.readInt()).filter(_ > Int.MinValue)
    new StudentRec(first, last, grades)
  }

  private def writeStudent(dout: DataOutput, s: StudentRec): Unit = {
    dout.write(s.first.take(20).getBytes().padTo(20, 0.toByte))
    dout.write(s.last.take(20).getBytes().padTo(20, 0.toByte))
    s.grades.padTo(10, Int.MinValue).foreach(dout.writeInt)
  }

  @Test def writeStudents: Unit = {
    val frs = new FixedRecordSeq[StudentRec](file, 20 + 20 + 4 * 10, readStudent, writeStudent)
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

  @Test def writeStudentsLong: Unit = {
    val frs = new FixedRecordSeq[StudentRec](file, 20 + 20 + 4 * 10, readStudent, writeStudent)
    val students = Array.fill(100) {
      val first = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val last = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
      new StudentRec(first, last, grades)
    }
    for (i <- students.indices) frs(i) = students(i)
    assertEquals(students.length, frs.length)
    for (i <- students.indices) {
      if (students(i).first.length > 20 || students(i).last.length > 20) {
        assertEquals(students(i).copy(first = students(i).first.take(20), last = students(i).last.take(20)), frs(i))
      } else {
        assertEquals(students(i), frs(i))
      }
    }
  }
}