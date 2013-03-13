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

import scala.language.higherKinds

import scala.concurrent.Future

import scalaz.Id.Id

import util.Events

sealed trait System[X[+_]] {
  def client: Client[X]
  def stopped: Events[Unit]
  def shutdown()
}

object System {

  trait Local extends System[Id]
              with origin.FinderComponent.Local
              with family.FinderComponent
              with origin.FactoryComponent
              with Processing
              with Processing.Default.Conversions
              with Processing.Default.Operations {

    override def client: Client = _client
    override def shutdown() {
      process.shutdown()
    }

    trait Client extends Client.Local with amber.origin.FinderComponent.Delegator.Local {
      override protected val finder = Local.this
    }

    private object _client extends Client
  }

  trait Remote extends System[Future] with origin.FinderComponent.Remote {

    override def client: Client = _client
    override def shutdown() {}

    trait Client extends Client.Remote with amber.origin.FinderComponent.Delegator.Remote {

      override protected type Configuration = amber.origin.FinderComponent.Remote.Configuration
      override protected def configuration: Configuration = Remote.this.configuration

      override protected val finder = Remote.this
    }

    private object _client extends Client
  }
}
