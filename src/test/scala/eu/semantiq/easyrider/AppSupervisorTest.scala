package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}

class AppSupervisorTest extends TestKit(ActorSystem("AppSupervisorTest")) with ImplicitSender with FunSpecLike with Matchers {

}
