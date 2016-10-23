package networking.chat

import java.rmi.Naming
import java.rmi.RemoteException
import java.rmi.server.UnicastRemoteObject
import javax.management.remote.rmi.RMIServerImpl
import scala.collection.mutable
import java.rmi.registry.LocateRegistry

object RMIServer extends UnicastRemoteObject with App with RemoteServer {
  LocateRegistry.createRegistry(1099)
  Naming.rebind("ChatServer", this)

  private val clients = mutable.Buffer[RemoteClient]()
  private var history = mutable.ListBuffer("Server Started\n")

  def connect(client: RemoteClient): String = {
    clients += client
    sendUpdate
    history.mkString("\n")+"\n"
  }

  def disconnect(client: RemoteClient) {
    clients -= client
    sendUpdate
  }

  def getClients: Seq[RemoteClient] = clients

  def publicMessage(client: RemoteClient, text: String) {
    history += client.name+" : "+text
    if (history.length > 10) history.remove(0)
  }

  private def sendUpdate: Unit = {
    val deadClients = clients.filter(c =>
      try {
        c.name
        false
      } catch {
        case ex: RemoteException => true
      })
    clients --= deadClients
    clients.foreach(_.clientUpdate(clients))
  }
}