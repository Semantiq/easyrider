package easyrider.business.core

import akka.actor.{ActorRef, ActorSystem}

class CoreModule(actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(), "EventBus")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus), "ApplicationManager")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  def apiFactory(client: ActorRef) = ApiActor(eventBus, applicationManager, componentManager)(client)
}
