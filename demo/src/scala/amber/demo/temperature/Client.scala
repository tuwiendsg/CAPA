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
import scala.util.Try

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.ActorSystem

import scalaz.std.string._
import scalaz.syntax.equal._

import util.{Logging, Scheduling}
import util.Value._

trait Client extends akka.System.Remote with Scheduling with Runnable {
  this: Logging =>

  override protected type Configuration = Client.Configuration
  override protected def configuration: Configuration = _configuration
  private object _configuration extends Configuration

  map[Double, Double]("temperature/celsius", "temperature/kelvin") {x => x + 273.15}
  map[Double, Double]("temperature/celsius", "temperature/fahrenheit") {x => x * 9 / 5 + 3}

  override def client: Client = _client
  trait Client extends super.Client {

    import Selections.exact

    val temperature = entity("Temperature")
    temperature.celsius = {() =>
      for {
        values <- readAll[Double](exact("temperature/celsius") where {
                    meta => for {location <- meta.location.as[String]} yield location === "A"
                  })
      } yield Try {values.min}.toOption
    }
    temperature.kelvin = {() =>
      for {
        values <- readAll[Double](exact("temperature/kelvin") where {
                    meta => for {location <- meta.location.as[String]} yield location === "B"
                  })
      } yield Try {values.max}.toOption
    }
    temperature.where {
      entity =>
        for {
          celsius <- entity.celsius.as[Double]
          kelvin <- entity.kelvin.as[Double]
        } yield celsius < kelvin
    }

    def readTemperature(): Future[String] = readOne(temperature.build()) map {
      _.fold(s"No ${client.temperature.name}") {_.toString}
    }
  }

  override def shutdown() {
    super[Scheduling].shutdown()
    super[Remote].shutdown()
    configuration.local.shutdown()
  }

  override def run() {
    println(configuration.delimiter)
    every(configuration.period) {() =>
      println(Await.result(client.readTemperature(), configuration.timeout))
      println(configuration.delimiter)
    }
  }

  private object _client extends Client
}

object Client {
  trait Configuration extends Scheduling.Configuration with akka.System.Remote.Configuration {

    val name: String = "temperature-client"
    val delimiter: String = "-" * 40
    def period: FiniteDuration = 2.seconds

    override val local = ActorSystem(name, ConfigFactory.load.getConfig(name))
    override val remote = "akka://temperature-server@127.0.0.1:2552"
  }
}
