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

package amber
package demo
package temperature

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.sys.process._
import scala.util.Try

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.ActorSystem

import scalaz.std.string._
import scalaz.syntax.equal._

import amber.util.{Logging, Scheduling}
import amber.util.Value._

trait Client extends akka.System.Remote with Scheduling with Runnable {
  this: Logging =>

  override protected type Configuration = Client.Configuration
  override protected def configuration: Configuration = _configuration
  private object _configuration extends Configuration

  map[BigDecimal, BigDecimal]("temperature/celsius", "temperature/kelvin") {x => x + 273.15}
  map[BigDecimal, BigDecimal]("temperature/celsius", "temperature/fahrenheit") {x => x * 9 / 5 + 3}

  override def client: Client = _client
  trait Client extends super.Client {

    val temperature = entity("Temperature")
    temperature.celsius = {
      () => for {values <- temperature("celsius")} yield Try {values.max}
    }
    temperature.kelvin = {
      () => for {values <- temperature("kelvin")} yield Try {values.max}
    }

    def notifyUser() {
      readOne(temperature.build()) map {
        _ map {
          temperature =>
            println(temperature)
            val celsius = temperature.celsius.as[BigDecimal].get
            if (celsius >= 50)
              Seq(
                "terminal-notifier",
                "-title", "\"" + _configuration.name + '"',
                "-message", "\"Temperature is " + celsius + '"'
              ).!
            println(_configuration.delimiter)
        }
      }
    }

    private def temperature(name: String) =
      readAll[BigDecimal](Selections.exact(s"temperature/$name"))
  }

  override def shutdown() {
    super[Scheduling].shutdown()
    super[Remote].shutdown()
    configuration.local.shutdown()
  }

  override def run() {
    println(configuration.delimiter)
    every(configuration.period) {() => client.notifyUser()}
  }

  private object _client extends Client
}

object Client {
  trait Configuration extends Scheduling.Configuration with akka.System.Remote.Configuration {

    val name: String = "raspberry-pi-client"
    val delimiter: String = "-" * 40
    def period: FiniteDuration = 2.seconds

    override val local = ActorSystem(name, ConfigFactory.load.getConfig(name))
    override val remote = "akka.tcp://raspberry-pi@192.168.0.28:2552"
  }
}
