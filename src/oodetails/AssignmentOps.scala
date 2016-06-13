package oodetails

object AssignmentOps {
  def main(args: Array[String]): Unit = {
    var v1 = new Vect2D_b(1, 2)
    val v2 = new Vect2D_b(2, 2)
    v1 += v2    // using v1 = v1.+(v2)
    
    val mv1 = new MutableVect2D_b(1, 2)
    val mv2 = new MutableVect2D_b(2, 2)
    mv1 += mv2  // using mv1.+=(mv2)
  }
}