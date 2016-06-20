package iostreams

import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream

object SerializationCode extends App {

  // Demonstrate error for non-serializable types.
  {
    class OtherData(val id: String, val course: String)
    class Student(val name: String, val grades: Array[Int], val od: OtherData) extends Serializable

    val oos = new ObjectOutputStream(new FileOutputStream("fail.bin"))
    val s = new Student("John", Array(98, 90), new OtherData("0123", "CS2"))
    oos.writeObject(s)
    oos.close()
  }

  // Demonstrate @transient
  {
    class OtherData(val id: String, val course: String)
    class Student(val name: String,
                  val grades: Array[Int],
                  @transient val od: OtherData) extends Serializable

    val oos = new ObjectOutputStream(new FileOutputStream("pass.bin"))
    val s = new Student("John", Array(98, 90), new OtherData("0123", "CS2"))
    oos.writeObject(s)
    oos.close()

    val ois = new ObjectInputStream(new FileInputStream("pass.bin"))
    ois.readObject() match {
      case s2: Student => println(s2.name+" "+s2.grades+" "+s2.od)
      case _ => println("Unknown type read.")
    }
    ois.close()
  }

  // Better Student with @transient
  {
    class OtherData(val id: String, val course: String)
    class Student(val name: String,
                  val grades: Array[Int],
                  @transient private var _od: OtherData) extends Serializable {
      assert(_od != null)
      def od: OtherData = {
        if (_od == null) {
          _od = new OtherData("012345", "Default")
        }
        _od
      }
    }

    val oos = new ObjectOutputStream(new FileOutputStream("pass.bin"))
    val s = new Student("John", Array(98, 90), new OtherData("0123", "CS2"))
    oos.writeObject(s)
    oos.close()

    val ois = new ObjectInputStream(new FileInputStream("pass.bin"))
    ois.readObject() match {
      case s2: Student => println(s2.name+" "+s2.grades+" "+s2.od)
      case _ => println("Unknown type read.")
    }
    ois.close()
  }

  // Student using custom serialization
  {
    class OtherData(val id: String, val course: String)
    class Student(val name: String,
                  val grades: Array[Int],
                  @transient private var _od: OtherData) extends Serializable {
      def od = _od
      private def writeObject(oos: ObjectOutputStream) {
        oos.defaultWriteObject()
        oos.writeUTF(od.id)
        oos.writeUTF(od.course)
      }
      private def readObject(ois: ObjectInputStream) {
        ois.defaultReadObject()
        _od = new OtherData(ois.readUTF, ois.readUTF)
      }
    }

    val oos = new ObjectOutputStream(new FileOutputStream("pass.bin"))
    val s = new Student("John", Array(98, 90), new OtherData("0123", "CS2"))
    oos.writeObject(s)
    oos.close()

    val ois = new ObjectInputStream(new FileInputStream("pass.bin"))
    ois.readObject() match {
      case s2: Student => println(s2.name+" "+s2.grades+" "+s2.od)
      case _ => println("Unknown type read.")
    }
    ois.close()
  }
}