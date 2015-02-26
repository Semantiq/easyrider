package easyrider.business.http

import akka.actor.{Stash, ActorRef, Props, Actor}
import easyrider.Api.{Authentication, AuthenticateUser}
import easyrider.Applications.ApplicationId
import easyrider.CommandDetails
import easyrider.Repository._
import spray.http.HttpHeaders.Authorization
import spray.http._

class UploadHandler(apiFactory: ActorRef => Props, repositoryStorage: ActorRef) extends Actor with Stash {
  override def receive: Receive = {
    case r: ChunkedRequestStart =>
      val Some(application) =  r.message.uri.query.get("application")
      val Some(version) =  r.message.uri.query.get("version")
      val api = context.actorOf(apiFactory(self))
      val credentials: BasicHttpCredentials = r.message.header[Authorization].get.credentials.asInstanceOf[BasicHttpCredentials]
      api ! AuthenticateUser(credentials.username, credentials.password)
      repositoryStorage ! StartUpload(CommandDetails(), Version(ApplicationId(application), version))
      context.become(authenticating(api))
  }

  def authenticating(api: ActorRef): Receive = {
    case e: Authentication =>
      context.stop(api)
      context.become(initiating)
      unstashAll()
    case _ => stash()
  }

  def initiating: Receive = {
    case Upload(upload) =>
      context.become(uploading(upload))
      unstashAll()
    case _ => stash()
  }

  def uploading(upload: ActorRef): Receive = {
    case r: MessageChunk => upload ! UploadChunk(r.data.toByteString)
    case r: ChunkedMessageEnd =>
      sender ! HttpResponse(entity = "OK")
      upload ! UploadCompleted()
  }
}

object UploadHandler {
  def apply(apiFactory: ActorRef => Props, repositoryStorage: ActorRef) = Props(classOf[UploadHandler], apiFactory, repositoryStorage)
}
