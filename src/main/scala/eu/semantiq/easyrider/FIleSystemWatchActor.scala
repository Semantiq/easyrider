package eu.semantiq.easyrider

import akka.actor.{ActorLogging, ActorRef, Actor}
import java.io.File
import java.nio.file._
import java.util.concurrent.TimeUnit
import scala.collection.JavaConversions._

class FileSystemWatchActor extends Actor with ActorLogging {
  import FileSystemWatchActor._
  private case class Subscription(file: File, watchKey: WatchKey, listener: ActorRef)

  val service = FileSystems.getDefault.newWatchService
  val watcherThread = new Thread(new Runnable {
    def run {
      while(!Thread.interrupted()) {
        try {
          val key = service.poll(100, TimeUnit.MILLISECONDS)
          if (key != null) {
            key.pollEvents.map {
              event: WatchEvent[_] =>
                if (event.kind != StandardWatchEventKinds.OVERFLOW) {
                  self ! WatchKeyAlerted(key, event)
                }
            }
            key.pollEvents.clear()
            key.reset
          }
        }
        catch {
          case e: Exception => self ! e
        }
      }
    }
  })
  watcherThread.start

  override def postStop {
    watcherThread.interrupt()
    watcherThread.join
    service.close
  }

  def receive = watching(Set[Subscription]())

  def watching(files: Set[Subscription]): Receive = {
    case Subscribe(f, to) =>
      log.info("Subscribed {} for changes", f)
      def watch(e: WatchEvent.Kind[Path]) = {
        val directory = if(f.isDirectory)
          f
        else
          f.getParentFile
        val path = directory.toPath
        path.register(service, e)
      }
      context.become(watching(files + Subscription(f, watch(StandardWatchEventKinds.ENTRY_MODIFY), to)))
    case Unsubscribe(f, to) =>
      val unsubscribed = files.filter { s =>
        s.listener == to && s.file == f
      }
      unsubscribed.foreach(_.watchKey.cancel)
      context.become(watching(files.filterNot(unsubscribed.contains)))
    case WatchKeyAlerted(key, event) =>
      files.filter(_.watchKey == key).foreach { s =>
        log.info("File changed {}", s.file)
        s.listener ! FileModified(s.file)
      }
  }
}
object FileSystemWatchActor {
  case class Subscribe(file: File, to: ActorRef)
  case class Unsubscribe(file: File, to: ActorRef)
  private case class WatchKeyAlerted(key: WatchKey, event: WatchEvent[_])
  case class FileModified(f: File)
}