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
package demo
package temperature

import scala.collection.immutable.Set

import scalaz.syntax.std.option._

import util.{ConfigurableComponent, Duration, Logging, Scheduling}

trait Demo extends Runnable with Scheduling with ConfigurableComponent {
  this: Logging =>

  def system: System
  def client: temperature.Client = system.client

  trait System extends temperature.System with Logging.Delegator {

    override protected lazy val logging = Demo.this
    override def client: Client = _client

    trait Client extends super.Client with temperature.Client with Logging.Delegator {
      override protected lazy val logging = System.this
    }

    private object _client extends Client
  }

  override def configuration: Configuration = _configuration

  trait Configuration {
    val period: Duration = 2 seconds
  }

  private object _configuration extends Configuration

  protected val delimiter = "-" * 40
  protected val locations = Set("A", "B")
  protected val origins = 5

  override def run() {
    try {
      for {location <- locations; _ <- 1 to origins} system.Temperature.createCelsius(location)
      println(delimiter)
      every(configuration.period) {() =>
        println((client.readTemperature() map {_.toString}) | ("No " + client.temperature.name))
        println(delimiter)
      }
      readLine()
    } finally {
      shutdown()
      system.shutdown()
    }
  }
}
