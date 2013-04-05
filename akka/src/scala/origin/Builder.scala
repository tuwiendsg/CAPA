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

import scala.collection.JavaConversions._

import _root_.akka.actor.{ActorRef, ActorSystem, Props}

import amber.util.{ConfigurableComponent, Type}

trait BuilderComponent extends amber.origin.BuilderComponent
                       with ConfigurableComponent {

  override protected type Configuration <: BuilderComponent.Configuration

  override protected type Origin[+A] = Origin.Local[A]
  override protected def builder: OriginBuilder = _builder

  private object _builder extends OriginBuilder {
    override def build[A: Type](name: amber.Origin.Name, family: amber.Origin.Family)
                               (_read: OriginBuilder.Read[A]) =
      new Origin(name, family) {

        override private[akka] val actor: ActorRef = configuration.system.actorOf(
          Props(new Origin.Actor(this)).withDispatcher("amber.origins.dispatcher")
        )

        override def read() = _read(amber.Origin.MetaInfo(meta))
      }
  }
}

object BuilderComponent {
  trait Configuration {
    def system: ActorSystem
  }
}
