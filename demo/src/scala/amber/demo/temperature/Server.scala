/*
 * Copyright 2013 Sanjin Sehic
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

package amber
package demo
package temperature

import scala.collection.immutable.Set
import scala.util.Random

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.ActorSystem

import amber.util.Logging

trait Server extends akka.System.Local with Runnable {
  this: Logging =>

  override protected type Configuration = Server.Configuration
  override protected def configuration: Configuration = _configuration
  private object _configuration extends Configuration

  object Temperature {
    def createCelsius(location: String): Origin.Local[Int] = {
      val temperature = origin.create("temperature/celsius") {
        () => Random.nextInt(55) - 15
      }
      temperature("location") = location
      temperature
    }
  }

  override def shutdown() {
    super.shutdown()
    configuration.system.shutdown()
  }

  override def run() {
    for {location <- configuration.locations; _ <- 1 to configuration.origins}
      Temperature.createCelsius(location)
  }
}

object Server {
  trait Configuration extends akka.System.Local.Configuration {

    val name: String = "temperature-server"
    val locations: Set[String] = Set("A", "B")
    val origins: Int = 5

    override val system = ActorSystem(name, ConfigFactory.load.getConfig(name))
  }
}
