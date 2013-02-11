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

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import _root_.akka.actor.{ActorSystem, Props}

import amber.util.{ConfigurableComponent, Filter, Logging}

trait BuilderComponent extends amber.origin.BuilderComponent with ConfigurableComponent {
  this: Logging =>

  override type Configuration <: BuilderComponent.Configuration

  override protected type Origin[+A] = OriginRef[A]
  override protected def builder: super.OriginBuilder = _builder

  protected trait OriginBuilder extends super.OriginBuilder {
    override def build[A: ClassTag : TypeTag, B: Origin.Read[A]#apply](name: Origin.Name,
                                                                       family: Origin.Family,
                                                                       read: B) = {
      val log = logger.create("amber.akka.Origin(" + name + ")")
      OriginRef[A](configuration.system.actorOf(Props(read match {
        case f: Origin.Read.Unfiltered[A] =>
          new OriginActor(name, family)(log) {
            override protected def read(filter: Filter[Origin.Meta.Readable]) =
              if (filter(meta)) f() else None
          }
        case f: Origin.Read.Filtered[A] =>
          new OriginActor(name, family)(log) {
            override protected def read(filter: Filter[Origin.Meta.Readable]) = f(filter)
          }
        }).withDispatcher("amber.origins.dispatcher")))(configuration.timeout)
    }
  }

  private object _builder extends OriginBuilder
}

object BuilderComponent {
  trait Configuration {
    def system: ActorSystem
    def timeout: FiniteDuration = FiniteDuration(
      system.settings.config.getMilliseconds("akka.actor.typed.timeout"),
      MILLISECONDS
    )
  }
}
