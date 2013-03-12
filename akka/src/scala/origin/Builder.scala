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

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

import _root_.akka.actor.{ActorSystem, Props}
import _root_.akka.util.Timeout

import scalaz.OptionT

import amber.util.{ConfigurableComponent, Type}

object BuilderComponent {

  trait Local extends amber.origin.BuilderComponent.Local with ConfigurableComponent {

    override protected type Configuration <: Local.Configuration

    override protected type Origin[+A] = Origin.Local[A]
    override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                 (_read: OriginBuilder.Read[A]) =
        new Origin(name, family) {

        override private[akka] lazy val actor = configuration.system.actorOf(
          Props(new Origin.Actor.Local(this)).withDispatcher("amber.origins.dispatcher")
        )

        override def read() = _read(meta)
      }
    }
  }

  object Local {
    trait Configuration {
      def system: ActorSystem
    }
  }

  trait Remote extends amber.origin.BuilderComponent.Remote with ConfigurableComponent {

    override protected type Origin[+A] = Origin.Remote[A]
    override protected type Configuration <: Remote.Configuration

    override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {

      implicit private def context = configuration.context
      implicit private def timeout: Timeout = configuration.timeout

      override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                 (_read: OriginBuilder.Read[A]) =
        new Origin.Remote(name, family) with Origin.MetaInfo.Local {

          override private[akka] lazy val actor = configuration.local.actorOf(
            Props(new Origin.Actor.Remote[A](this)).withDispatcher("amber.origins.dispatcher")
          )

          override def read() = _read(meta)

          override def selectDynamic(name: Origin.MetaInfo.Name) =
            OptionT(Future.successful(select(name)))
        }
    }
  }

  object Remote {
    trait Configuration extends amber.origin.BuilderComponent.Remote.Configuration {

      def local: ActorSystem

      override def context: ExecutionContext = local.dispatcher
      def timeout: FiniteDuration = FiniteDuration(
        local.settings.config.getMilliseconds("akka.actor.typed.timeout"),
        MILLISECONDS
      )
    }
  }
}
