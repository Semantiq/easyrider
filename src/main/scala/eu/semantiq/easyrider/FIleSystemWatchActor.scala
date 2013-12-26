package eu.semantiq.easyrider

import akka.actor.{ActorLogging, ActorRef, Actor}
import java.io.File
import org.apache.commons.io.monitor.{FileAlterationMonitor, FileAlterationListenerAdaptor, FileAlterationObserver}

class FileSystemWatchActor extends Actor with ActorLogging {
  import FileSystemWatchActor._
  private case class Subscription(file: File, observer: FileAlterationObserver, listener: ActorRef)

  private var subscriptions = Set[Subscription]()
  private val monitor = new FileAlterationMonitor()
  monitor.start()

  override def postStop() {
    subscriptions.foreach(s => s.observer.destroy())
    monitor.stop()
  }

  def receive: Receive = {
    case Subscribe(f, to) =>
      log.info("Subscribed {} for changes in {}", to, f)
      val observer = new FileAlterationObserver(f)
      monitor.addObserver(observer)
      observer.addListener(new FileAlterationListenerAdaptor() {
        override def onFileChange(file: File) {
          log.info("Change detected in {}", file.getPath)
          to ! FileModified(file)
        }
      })
      subscriptions += Subscription(f, observer, to)
    case UnSubscribe(f, to) =>
      subscriptions.find(s => s.file == f && s.listener == to) match {
        case Some(s) =>
          monitor.removeObserver(s.observer)
          s.observer.destroy()
          subscriptions -= s
        case None => // trying to un-subscribe from non existing subscription
      }
  }
}

object FileSystemWatchActor {
  case class Subscribe(file: File, to: ActorRef)
  case class UnSubscribe(file: File, to: ActorRef)
  case class FileModified(f: File)
}