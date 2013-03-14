/*
 * Copyright 2012 Sanjin Sehic
 *

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
package akka

import _root_.akka.actor.PoisonPill

import amber.util.{ConfigurableComponent, Logging}
import akka.util.EventSource

object System {

  trait Local extends amber.System.Local
               with akka.origin.BuilderComponent.Local
               with amber.origin.BuilderComponent.Logging.Local
               with akka.origin.FinderComponent.Local
               with amber.family.FinderComponent.Default
               with akka.origin.FactoryComponent.Local
               with ConfigurableComponent {
    this: Logging =>

    override protected type Configuration <: Local.Configuration

    @transient private[this] val log = logger.create("amber.akka.System.Local")

    override val stopped = EventSource[Unit](configuration.system)
    override def origin: super.OriginFactory = _origin
    override protected val actor: Actor = _actor

    override def shutdown() {
      log.info("Shutting down")
      super.shutdown()
      actor.finder ! PoisonPill
      actor.factory ! PoisonPill
      stopped.emit(())
      log.info("Shutdown successful")
    }

    trait OriginFactory extends super.OriginFactory with OriginFactory.Logging {
      @transient override protected val log = logger.create("amber.akka.origin.Factory")
    }

    protected trait Actor extends akka.origin.FinderComponent.Actor
                          with akka.origin.FactoryComponent.Actor

    private object _origin extends OriginFactory
    private object _actor extends Actor {
      override val factory = akka.origin.FactoryComponent.Actor.local(system)(Local.this)
      override val finder = akka.origin.FinderComponent.Actor.local(system)(Local.this)
      private def system = configuration.system
    }
  }

  trait Remote extends amber.System.Remote
               with akka.origin.BuilderComponent.Remote
               with amber.origin.BuilderComponent.Logging.Remote
               with akka.origin.FinderComponent.Remote
               with akka.origin.FactoryComponent.Remote
               with ConfigurableComponent {
    this: Logging =>

    override protected type Configuration <: Remote.Configuration

    @transient private[this] val log = logger.create("amber.akka.System.Remote")

    override val stopped = EventSource[Unit](configuration.local)
    override protected val actor: Actor = _actor

    override def shutdown() {
      log.info("Shutting down")
      super.shutdown()
      actor.finder ! PoisonPill
      actor.factory ! PoisonPill
      stopped.emit(())
      log.info("Shutdown successful")
    }

    protected trait Actor extends akka.origin.FinderComponent.Actor
                          with akka.origin.FactoryComponent.Actor

    private object _actor extends Actor {
      override val factory = akka.origin.FactoryComponent.Actor.remote(system)(Remote.this)
      override val finder = akka.origin.FinderComponent.Actor.remote(system)(Remote.this)
      private def system = configuration.local
    }
  }

  object Local {
    trait Configuration extends akka.origin.BuilderComponent.Local.Configuration
                        with akka.origin.FactoryComponent.Local.Configuration
  }

  object Remote {
    trait Configuration extends akka.origin.FinderComponent.Remote.Configuration
                        with akka.origin.FactoryComponent.Remote.Configuration
  }
}
