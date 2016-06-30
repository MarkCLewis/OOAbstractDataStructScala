package test.binaryfiles.adt

import java.io.DataInput
import java.io.DataOutput
import java.io.File

import org.junit.Assert._
import org.junit.Before
import org.junit.Test

import binaryfiles.adt.FixedRecordList

class TestFixedRecordList {
  private var file: File = null
  private case class PointRec(x: Double, y: Double, z: Double, d: Int)
  private case class StudentRec(first: String, last: String, grades: List[Int])

  @Before def setFile: Unit = {
    file = new File("TextFile.bin")
    file.deleteOnExit()
    if (file.exists()) file.delete()
  }

  @Test def emptyStart: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    assertTrue(frl.length == 0)
  }

  @Test def write4: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
  }

  @Test def write4closeOpen: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.close()
    val frl2 = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    assertEquals(4, frl2.length)
    assertEquals(3, frl2(0))
    assertEquals(8, frl2(1))
    assertEquals(2, frl2(2))
    assertEquals(9, frl2(3))
  }

  @Test def write2RemoveAllWrite4: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8
    assertEquals(2, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    frl.remove(0)
    assertEquals(1, frl.length)
    frl.remove(0)
    assertEquals(0, frl.length)
    frl += 3 += 8
    2 +=: 9 +=: frl
    assertEquals(4, frl.length)
    assertEquals(2, frl(0))
    assertEquals(9, frl(1))
    assertEquals(3, frl(2))
    assertEquals(8, frl(3))
  }

  @Test def prepend4: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    3 +=: 8 +=: 2 +=: 9 +=: frl
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
  }

  @Test def write4remove: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.remove(1)
    assertEquals(3, frl.length)
    assertEquals(3, frl(0))
    assertEquals(2, frl(1))
    assertEquals(9, frl(2))
  }

  @Test def write4removeHead: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.remove(0)
    assertEquals(3, frl.length)
    assertEquals(8, frl(0))
    assertEquals(2, frl(1))
    assertEquals(9, frl(2))
    5 +=: frl
    assertEquals(4, frl.length)
    assertEquals(5, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
  }

  @Test def write4removeTail: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.remove(3)
    assertEquals(3, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    frl += 5
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(5, frl(3))
  }

  @Test def write4insert: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.insertAll(1, List(10, 11, 12))
    assertEquals(7, frl.length)
    assertEquals(3, frl(0))
    assertEquals(10, frl(1))
    assertEquals(11, frl(2))
    assertEquals(12, frl(3))
    assertEquals(8, frl(4))
    assertEquals(2, frl(5))
    assertEquals(9, frl(6))
  }

  @Test def write4insertHead: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.insertAll(0, List(10, 11, 12))
    assertEquals(7, frl.length)
    assertEquals(10, frl(0))
    assertEquals(11, frl(1))
    assertEquals(12, frl(2))
    assertEquals(3, frl(3))
    assertEquals(8, frl(4))
    assertEquals(2, frl(5))
    assertEquals(9, frl(6))
  }

  @Test def write4insertTail: Unit = {
    val frl = new FixedRecordList[Int](file, din => din.readInt(), (dout, i) => dout.writeInt(i))
    frl += 3 += 8 += 2 += 9
    assertEquals(4, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    frl.insertAll(4, List(10, 11, 12))
    assertEquals(7, frl.length)
    assertEquals(3, frl(0))
    assertEquals(8, frl(1))
    assertEquals(2, frl(2))
    assertEquals(9, frl(3))
    assertEquals(10, frl(4))
    assertEquals(11, frl(5))
    assertEquals(12, frl(6))
  }

  @Test def write100: Unit = {
    val frl = new FixedRecordList[Double](file, din => din.readDouble(), (dout, d) => dout.writeDouble(d))
    val nums = Array.fill(100)(math.random)
    for (i <- nums.indices) frl += nums(i)
    assertEquals(nums.length, frl.length)
    for (i <- nums.indices) assertEquals(nums(i), frl(i), 0)
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
    val frl = new FixedRecordList[PointRec](file, readPoint, writePoint)
    val pnts = Array.fill(10)(new PointRec(math.random, math.random, math.random, util.Random.nextInt))
    for (i <- pnts.indices) frl += pnts(i)
    assertEquals(pnts.length, frl.length)
    for (i <- pnts.indices) assertEquals(pnts(i), frl(i))
  }

  @Test def rewritePoints: Unit = {
    val frl = new FixedRecordList[PointRec](file, readPoint, writePoint)
    val pnts = Array.fill(10)(new PointRec(math.random, math.random, math.random, util.Random.nextInt))
    for (i <- pnts.indices) frl += pnts(i)
    assertEquals(pnts.length, frl.length)
    for (i <- pnts.indices) assertEquals(pnts(i), frl(i))
    for (i <- 0 until pnts.length / 2) {
      val index = util.Random.nextInt(pnts.length)
      pnts(index) = new PointRec(math.random, math.random, math.random, util.Random.nextInt)
      frl(index) = pnts(index)
    }
    for (i <- pnts.indices) assertEquals(pnts(i), frl(i))
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
    val frl = new FixedRecordList[StudentRec](file, readStudent, writeStudent)
    val students = Array.fill(100) {
      val first = Array.fill(util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val last = Array.fill(util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
      new StudentRec(first, last, grades)
    }
    for (i <- students.indices) frl += students(i)
    assertEquals(students.length, frl.length)
    for (i <- students.indices) assertEquals(students(i), frl(i))
  }

  @Test def writeStudentsLong: Unit = {
    val frl = new FixedRecordList[StudentRec](file, readStudent, writeStudent)
    val students = Array.fill(100) {
      val first = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val last = Array.fill(10 + util.Random.nextInt(15))(('a' + util.Random.nextInt(26)).toChar).mkString
      val grades = List.fill(util.Random.nextInt(5) + 6)(60 + util.Random.nextInt(40))
      new StudentRec(first, last, grades)
    }
    for (i <- students.indices) frl += students(i)
    assertEquals(students.length, frl.length)
    for (i <- students.indices) {
      if (students(i).first.length > 20 || students(i).last.length > 20) {
        assertEquals(students(i).copy(first = students(i).first.take(20), last = students(i).last.take(20)), frl(i))
      } else {
        assertEquals(students(i), frl(i))
      }
    }
  }
}