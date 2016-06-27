package recursion.drawing

import java.rmi.RemoteException
import java.rmi.server.UnicastRemoteObject

import scala.collection.mutable

@remote trait RemoteCollaborationServer {
  def joinCollaboration(col: RemoteCollaborator): (Array[RemoteCollaborator], Array[(String, Drawing)])
  def addDrawing(title: String, drawing: Drawing): Unit
}

class CollaborationServer extends UnicastRemoteObject with RemoteCollaborationServer {
  private val collaborators = mutable.Buffer[RemoteCollaborator]()
  private val drawings = mutable.Buffer[(String, Drawing)]()

  def joinCollaboration(col: RemoteCollaborator): (Array[RemoteCollaborator], Array[(String, Drawing)]) = {
    collaborators += col
    (collaborators.toArray, drawings.toArray)
  }

  def addDrawing(title: String, drawing: Drawing): Unit = {
    drawings += title -> drawing
    for (c <- collaborators) {
      try {
        c.addDrawing(title, drawing)
      } catch {
        case ex: RemoteException =>
      }
    }
  }
}