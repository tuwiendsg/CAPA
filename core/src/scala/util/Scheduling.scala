/*
 * Copyright 2012 Sanjin Sehic
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package at.ac.tuwien.infosys
package amber
package util

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.NANOSECONDS

import scala.concurrent.duration.Duration
import scala.util.Try

trait Scheduling extends ConfigurableComponent {
  this: Logging =>

  override protected type Configuration <: Scheduling.Configuration

  @transient private[this] val log: Logger = logger.create("amber.Scheduler")

  private val scheduler = Executors.newScheduledThreadPool(configuration.threads)

  def after(delay: Duration)(f: () => Unit) {
    scheduler.schedule(toRunnable(f), delay.length, delay.unit)
  }

  def every(period: Duration, delay: Duration = Duration.Zero)(f: () => Unit) {
    scheduler.scheduleAtFixedRate(toRunnable(f), delay.toNanos, period.toNanos, NANOSECONDS)
  }

  def shutdown() {scheduler.shutdown()}

  private def toRunnable(f: () => Unit) = new Runnable {
    override def run() {
      Try {f()}.recover {
        case ex: Exception => log.error("Scheduled task finished unsuccessfully!", Some(ex))
      }
    }
  }
}

object Scheduling {
  trait Configuration {
    def threads: Int = 1
  }
}
