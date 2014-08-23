package easyrider.business.core

import akka.actor.{ActorRef, ActorSystem}

class CoreModule(actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus())
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus))
  def apiFactory(client: ActorRef) = ApiActor(eventBus, applicationManager)(client)
}
