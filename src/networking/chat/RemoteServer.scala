package networking.chat

@remote trait RemoteServer {
  def connect(client: RemoteClient): String
  def disconnect(client: RemoteClient): Unit
  def getClients: Seq[RemoteClient]
  def publicMessage(client: RemoteClient, text: String): Unit
}