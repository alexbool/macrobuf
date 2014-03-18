package me.alexbool.macrobuf.benchmark

import scala.collection.JavaConversions._
import java.lang.management.GarbageCollectorMXBean
import javax.management.{Notification, NotificationListener, NotificationEmitter}
import com.sun.management.GarbageCollectionNotificationInfo

object GarbageCollectorLogger {

  val notificationListener = new NotificationListener {
    def handleNotification(n: Notification, handback: Any) {
      if (n.getType == GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION) {
        System.out.println(s"Warning: GC event (${n.getMessage})")
      }
    }
  }

  def enableGcLoggingToStdout() {
    val mBeans: Seq[GarbageCollectorMXBean] = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans
    mBeans.foreach(b => b.asInstanceOf[NotificationEmitter].addNotificationListener(notificationListener, null, null))
  }

  def disableGcLogging() {
    java.lang.management.ManagementFactory
      .getGarbageCollectorMXBeans
      .map(_.asInstanceOf[NotificationEmitter])
      .foreach(_.removeNotificationListener(notificationListener))
  }
}
