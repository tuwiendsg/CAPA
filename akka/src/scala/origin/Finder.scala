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

import _root_.akka.actor.{ActorRef, ActorSystem, Props}
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import amber.util.{ConfigurableComponent, NotNothing, Type}
import amber.util.MultiTrie.Selection

object FinderComponent {

  trait Local extends amber.origin.FinderComponent.Local {
    protected override type Origin[+A] <: Origin.Local[A]
  }

  trait Remote extends amber.origin.FinderComponent.Remote with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration

    override protected type Origin[+A] = Origin.Remote[A]
    override def origins: OriginFinder = _origins

    private object _origins extends OriginFinder {

      import Message.{Request, Response}

      implicit private def context = configuration.system.dispatcher
      implicit private def timeout: Timeout = configuration.timeout
      private def reference = configuration.reference

      override def find[A: NotNothing](selection: Selection)(implicit typeA: Type[A]) =
        ask(reference, Request.Find[A](selection)).mapTo[Response.Find] map {
          result =>
            for {(name, family, ttype, actor) <- result if ttype <:< typeA}
              yield (new Origin(name, family)(actor)(timeout, ttype)).asInstanceOf[Origin[A]]
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

    import Message.Request
    import context.dispatcher

    override def receive = {
      case find: Request.Find[_] =>
        future {
          for {origin <- find(finder)}
            yield (origin.name, origin.family, origin.ttype, origin.actor)
        } pipeTo sender
    }
  }

  private sealed trait Message extends Serializable

  private object Message {

    object Request {
      case class Find[A: Type](selection: Selection) extends Message {
        def apply(finder: FinderComponent.Local) = finder.origins.find[A](selection)
      }
    }

    object Response {
      type Find = Set[(amber.Origin.Name, amber.Origin.Family, Type[_], ActorRef)]
    }
  }
}
