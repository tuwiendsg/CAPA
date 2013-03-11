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
package akka
package origin

import scala.language.higherKinds

import scala.collection.immutable.Set
import scala.concurrent.future
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.reflect.runtime.universe.TypeTag

import _root_.akka.actor.{ActorRef, ActorSystem, Props}
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import amber.util.ConfigurableComponent
import amber.util.MultiTrie.Selection

object FinderComponent {

  import Message.Request

  trait Local extends amber.origin.FinderComponent.Local {
    protected override type Origin[+A] <: Origin.Local[A]
  }

  trait Remote extends amber.origin.FinderComponent.Remote with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration

    override protected type Origin[+A] = Origin.Remote[A]
    override def origins: OriginFinder = _origins

    private object _origins extends OriginFinder {

      implicit private def context = configuration.system.dispatcher
      private def reference = configuration.reference
      private def timeout = configuration.timeout

      override def find(selection: Selection) =
        ask(reference, Request.Find(selection))(timeout).mapTo[Set[ActorRef]] map {
          actors => for {actor <- actors} yield new Origin.Remote[Any](actor)(timeout)
        }
    }
  }

  object Remote {
    trait Configuration {
      def system: ActorSystem
      def reference: ActorRef = system.actorFor("/user/origins-finder")
      def timeout: FiniteDuration = FiniteDuration(
        system.settings.config.getMilliseconds("akka.actor.typed.timeout"),
        MILLISECONDS
      )
    }
  }

  class Actor(finder: FinderComponent.Local) extends _root_.akka.actor.Actor {

    import context.dispatcher

    override def receive = {
      case Request.Find(selection) =>
        future {for {origin <- finder.origins.find(selection)} yield origin.actor} pipeTo sender
    }
  }

  private sealed trait Message extends Serializable

  private object Message {
    object Request {
      case class Find(selection: Selection) extends Message
    }
  }
}
