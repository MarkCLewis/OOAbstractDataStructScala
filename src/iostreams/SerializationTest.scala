package iostreams

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.BufferedInputStream
import java.io.ObjectOutputStream
import java.io.BufferedOutputStream

class Student(val name: String, val grades: Array[Int]) extends Serializable

object SerializationTest extends App {
  args(0) match {
    case "-r" =>
      val ois = new ObjectInputStream(new BufferedInputStream(new FileInputStream(args(1))))
      ois.readObject() match {
        case s: Student => println(s.name+" "+s.grades.mkString(", "))
        case _ => println("Unidentified type.")
      }
      ois.close()
    case "-w" =>
      val oos = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(args(1))))
      val s = new Student(args(2), args.drop(3).map(_.toInt))
      oos.writeObject(s)
      oos.close()
    case _ =>
      println("Usage: -r filename | -w filename name g1 g2 ...")
  }
}