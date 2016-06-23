package networking.chat

import scalafx.Includes._
import java.rmi.server.UnicastRemoteObject
import java.rmi.RemoteException
import java.rmi.Naming
import scalafx.application.JFXApp
import scalafx.scene.control.ListView
import scalafx.scene.control.TextArea
import scalafx.scene.control.TextField
import scalafx.scene.control.TextInputDialog
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.event.ActionEvent
import scalafx.scene.control.ScrollPane
import scalafx.stage.WindowEvent
import scalafx.scene.layout.BorderPane
import scalafx.collections.ObservableBuffer
import scalafx.application.Platform

object RMIClient extends UnicastRemoteObject with JFXApp with RemoteClient {
  val mDialog = new TextInputDialog("localhost")
  mDialog.title = "Server Machine"
  mDialog.contentText = "What server do you want to connect to?"
  mDialog.headerText = "Server Name"
  val (myName, server) = mDialog.showAndWait() match {
    case Some(machineName) =>
      Naming.lookup("rmi://"+machineName+"/ChatServer") match {
        case server: RemoteServer =>
          val nDialog = new TextInputDialog("")
          nDialog.title = "Chat Name"
          nDialog.contentText = "What name do you want to go by?"
          nDialog.headerText = "User Name"
          val name = nDialog.showAndWait()
          if (name.nonEmpty) (name.get, server)
          else sys.exit(0)
        case _ =>
          println("That machine does not have a registered server.")
          sys.exit(0)
      }
    case None => sys.exit(0)
  }

  private val chatText = new TextArea(server.connect(this))
  chatText.editable = false
  private var clients = server.getClients
  private val userList = new ListView(clients.map(_.name))
  private val chatField = new TextField()
  chatField.onAction = (ae: ActionEvent) => {
    if (chatField.text.value.trim.nonEmpty) {
      val recipients = if (userList.selectionModel.value.selectedItems.isEmpty) {
        server.publicMessage(RMIClient.this, chatField.text.value)
        clients
      } else {
        userList.selectionModel.value.selectedIndices.map(i => clients(i)).toSeq
      }
      recipients.foreach(r => try {
        r.message(RMIClient.this, chatField.text.value)
      } catch {
        case ex: RemoteException => chatText.text = chatText.text.value+"Couldn't send to one recipient."
      })
      chatField.text = ""
    }
  }

  stage = new PrimaryStage {
    title = "RMI Based Chat"
    scene = new Scene(600, 600) {
      val borderPane = new BorderPane
      val scrollList = new ScrollPane
      scrollList.content = userList
      borderPane.left = scrollList
      val nestedBorder = new BorderPane
      val scrollChat = new ScrollPane
      scrollChat.content = chatText
      nestedBorder.center = scrollChat
      nestedBorder.bottom = chatField
      borderPane.center = nestedBorder
      root = borderPane

      userList.prefHeight <== borderPane.height
      chatText.prefWidth <== nestedBorder.width
      chatText.prefHeight <== nestedBorder.height - chatField.height
    }
    onCloseRequest = (we: WindowEvent) => {
      server.disconnect(RMIClient.this)
      sys.exit(0)
    }
  }

  def name: String = myName

  def message(sender: RemoteClient, text: String): Unit = Platform.runLater {
    chatText.text = chatText.text.value + sender.name+" : "+text+"\n"
  }

  def clientUpdate(cls: Seq[RemoteClient]): Unit = Platform.runLater {
    clients = cls
    if (userList != null) userList.items = ObservableBuffer(cls.map(c =>
      try {
        c.name
      } catch {
        case ex: RemoteException => "Error"
      }))
  }
}