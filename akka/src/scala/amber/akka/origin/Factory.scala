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
package akka
package origin

import scala.language.higherKinds

import _root_.akka.actor.{ActorRef, ActorSelection, ActorSystem, Props}
import _root_.akka.util.Timeout

import amber.util.ConfigurableComponent
import amber.akka.util.EventSource

sealed trait FactoryComponent {
  this: amber.origin.FactoryComponent =>

  protected def actor: FactoryComponent.Actor
}

object FactoryComponent {

  trait Local extends amber.origin.FactoryComponent.Local.Default
              with FactoryComponent
              with ConfigurableComponent {

    override protected type Configuration <: Local.Configuration
    override protected type Origin[+A] <: Origin.Local[A]

    override def origin: OriginFactory = _origin

    trait OriginFactory extends super.OriginFactory {
      override def created: EventSource[Origin[_]] = _created
      private object _created extends EventSource[Origin[_]](configuration.system)
    }

    private object _origin extends OriginFactory
  }

  object Local {
    trait Configuration {
      def system: ActorSystem
    }
  }

  trait Remote extends BuilderComponent.Remote
               with amber.origin.FactoryComponent.Remote.Default
               with FactoryComponent
               with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration
    override protected type Origin[+A] <: Origin.Remote[A]

    override def origin: OriginFactory = _origin

    trait OriginFactory extends super.OriginFactory {
      override def created: EventSource[Origin[_]] = _created
      private object _created extends EventSource[Origin[_]](configuration.local) {
        private val receiver = configuration.local.actorOf(Props(new _root_.akka.actor.Actor {

          import Message.Request

          implicit private def ctx = configuration.context
          implicit private def timeout: Timeout = configuration.timeout

          override def preStart() {
            super.preStart()
            configuration.factory ! Request.Subscribe.Created(self)
          }

          override def receive = {
            case origin: Origin.Serialized[_] => emit(origin.toRemote(Remote.this))
          }
        }).withDispatcher("amber.observers.dispatcher"))
      }
    }

    private object _origin extends OriginFactory
  }

  object Remote {
    trait Configuration extends BuilderComponent.Remote.Configuration {
      def remote: String
      def factory: ActorSelection = local.actorSelection(s"$remote/user/${Actor.name}")
    }
  }

  trait Actor {
    def factory: ActorRef
  }

  object Actor {

    val name: String = "origin-factory"

    def local(system: ActorSystem)(factory: FactoryComponent.Local): ActorRef =
      system.actorOf(Props(new Local(factory)), name = name)

    def remote(system: ActorSystem)(factory: FactoryComponent.Remote): ActorRef =
      system.actorOf(Props(new Remote(factory)), name = name)

    private class Local(factory: FactoryComponent.Local) extends _root_.akka.actor.Actor {

      import Message.Request

      override def receive = {
        case Request.Subscribe.Created(reference) => factory.origin.created.subscribe(reference)
      }
    }

    private class Remote(factory: FactoryComponent.Remote) extends _root_.akka.actor.Actor {

      import Message.Request

      override def receive = {
        case Request.Subscribe.Created(reference) => factory.origin.created.subscribe(reference)
      }
    }
  }

  private sealed trait Message extends Serializable

  private object Message {

    object Request {
      object Subscribe {
        case class Created(reference: ActorRef) extends Message
      }
    }
  }
}
