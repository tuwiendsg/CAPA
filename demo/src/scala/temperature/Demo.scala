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

import scala.language.higherKinds

import scala.collection.immutable.Set
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import scalaz.Id.Id
import scalaz.syntax.std.option._

import util.{ConfigurableComponent, Logging, Scheduling}

sealed trait Demo[X[+_]] extends Runnable with Scheduling with ConfigurableComponent {
  this: Logging =>

  override protected type Configuration <: Demo.Configuration

  def system: amber.System[X] with System
  def client: Client = system.client

  protected def copoint[A](xa: X[A]): A

  trait System extends Logging.Delegator {
    this: amber.System[X] =>

    override def client: Client with Demo.this.Client

    override protected lazy val logging = Demo.this
  }

  trait Client extends Logging.Delegator {
    this: temperature.Client[X] =>

    override protected lazy val logging = Demo.this

    def start() {
      println(configuration.delimiter)
      every(configuration.period) {() =>
        println(copoint(readTemperature()).fold(s"No ${temperature.name}") {_.toString})
        println(configuration.delimiter)
      }
    }
  }

  override def shutdown() {
    super.shutdown()
    system.shutdown()
  }
}

object Demo {

  trait Configuration extends Scheduling.Configuration {
    val delimiter: String = "-" * 40
    def period: FiniteDuration = 2.seconds
  }

  trait Local extends Demo[Id] {
    this: Logging =>

    override protected type Configuration = Local.Configuration
    override protected def configuration: Configuration = _configuration
    private object _configuration extends Configuration

    override def system: System
    override protected def copoint[A](a: Id[A]) = a

    trait System extends super.System with temperature.System {

      override def client: Client with Local.this.Client = _client

      def start() {
        for {location <- configuration.locations; _ <- 1 to configuration.origins}
          system.Temperature.createCelsius(location)
      }

      private object _client extends Client with Local.this.Client with Client.Local
    }

    override def run() {
      try {
        system.start()
        client.start()
        readLine()
      } finally {
        shutdown()
      }
    }
  }

  object Local {
    trait Configuration extends Demo.Configuration {
      val locations: Set[String] = Set("A", "B")
      val origins: Int = 5
    }
  }

  trait Remote extends Demo[Future] {
    this: Logging =>

    override protected type Configuration = Remote.Configuration
    override protected def configuration: Configuration = _configuration
    private object _configuration extends Configuration

    override def system: System
    override protected def copoint[A](future: Future[A]) =
      Await.result(future, configuration.timeout)

    trait System extends super.System with amber.System.Remote {

      override def client: Client with Remote.this.Client = _client

      private object _client extends Client with Remote.this.Client with Client.Remote {
        override protected type Configuration = Client.Remote.Configuration
        override protected object configuration extends Client.Remote.Configuration {
          override def context = System.this.configuration.context
        }
      }
    }

    override def run() {
      try {
        client.start()
        readLine()
      } finally {
        shutdown()
      }
    }
  }

  object Remote {
    trait Configuration extends Demo.Configuration {
      def timeout: FiniteDuration = 1.second
    }
  }
}
