package easyrider.testing

import akka.actor.{ActorSystem, ActorRef, Props}
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp
import spray.can.websocket.frame.TextFrame
import spray.can.websocket.{Send, UpgradedToWebSocket, WebSocketClientWorker}
import spray.http.{HttpHeaders, HttpMethods, HttpRequest}


object WebSocketProbe {
  def apply(master: ActorRef): Props = {
    val ssl = false
    val host = "127.0.0.1"
    val port = 8080
    val headers = List(
      HttpHeaders.Host(host, port),
      HttpHeaders.Connection("Upgrade"),
      HttpHeaders.RawHeader("Upgrade", "websocket"),
      HttpHeaders.RawHeader("Sec-WebSocket-Version", "13"),
      HttpHeaders.RawHeader("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw=="),
      HttpHeaders.RawHeader("Sec-WebSocket-Extensions", "permessage-deflate"))

    val connect = Http.Connect(host, port, ssl)

    val request = HttpRequest(HttpMethods.GET, "/api", headers)
    Props(new WebSocketClient(connect, request, master))
  }

  private class WebSocketClient(connect: Http.Connect, val upgradeRequest: HttpRequest, master: ActorRef) extends WebSocketClientWorker {
    implicit val system = context.system
    IO(UHttp) ! connect

    def businessLogic: Receive = {
      case UpgradedToWebSocket =>
        context.become(connected orElse closeLogic)
        unstashAll()
      case _: Send => stash()
    }

    def connected: Receive = {
      case textFrame: TextFrame => master ! textFrame
      case send: Send => connection ! send.frame
    }
  }
}
