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

package at.ac.tuwien.infosys
package amber
package akka
package origin

import scala.language.higherKinds

import java.util.concurrent.TimeoutException

import scala.collection.immutable.Set
import scala.concurrent.Future

import _root_.akka.actor.{ActorRef, ActorSystem, Props}
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import scalaz.Id.Id

import amber.util.{ConfigurableComponent, MultiTrie, NotNothing, Type}
import amber.util.MultiTrie.Selection

sealed trait FinderComponent[X[+_]] {
  this: amber.origin.FinderComponent[X] =>

  protected def actor: FinderComponent.Actor
}

object FinderComponent {

  trait Local extends amber.origin.FinderComponent.Local.Default with FinderComponent[Id] {
    override protected type Origin[+A] <: Origin.Local[A]
  }

  trait Remote extends BuilderComponent.Remote
               with FinderComponent[Future]
               with amber.origin.FinderComponent.Remote.Default
               with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration
    override protected type Origin[+A] <: Origin.Remote[A]

    override def origins: OriginFinder = _origins
    private object _origins extends OriginFinder {

      import Message.Request

      implicit private def context = configuration.context
      implicit private def timeout: Timeout = configuration.timeout

      override def find[A: NotNothing](selection: Selection)(implicit typeA: Type[A]) =
        ask(configuration.finder, Request.Find[A](selection)).mapTo[Set[Origin.Serialized[_]]] map {
          for {origin <- _ if origin.returns[A]} yield origin.toRemote.asInstanceOf[Origin[A]]
        } recover {case _: TimeoutException => Set.empty} flatMap {
          remote => super.find[A](selection) map {_ ++ remote}
        }
    }
  }

  object Remote {
    trait Configuration extends BuilderComponent.Remote.Configuration
                        with amber.origin.FinderComponent.Remote.Default.Configuration {

      def remote: String
      def finder: ActorRef = local.actorFor(s"${remote}/user/${Actor.name}")
    }
  }

  trait Actor {
    def finder: ActorRef
  }

  object Actor {

    val name: String = "origins-finder"

    def local(system: ActorSystem)(finder: FinderComponent.Local): ActorRef =
      system.actorOf(Props(new Actor.Local(finder)), name = name)

    def remote(system: ActorSystem)(finder: FinderComponent.Remote): ActorRef =
      system.actorOf(Props(new Actor.Remote(finder)), name = name)

    private class Local(finder: FinderComponent.Local) extends _root_.akka.actor.Actor {

      import Message.Request
      import context.dispatcher

      override def receive = {
        case find: Request.Find[_] => sender ! find(finder)
      }
    }

    private class Remote(finder: FinderComponent.Remote) extends _root_.akka.actor.Actor {

      import Message.Request
      import context.dispatcher

      override def receive = {
        case find: Request.Find[_] => find(finder) pipeTo sender
      }
    }
  }

  private sealed trait Message extends Serializable

  private object Message {
    object Request {
      case class Find[A: Type](selection: Selection) extends Message {
        def apply[X[+_]](finder: amber.origin.FinderComponent[X]) =
          finder.origins.find[A](selection)
      }
    }
  }
}
