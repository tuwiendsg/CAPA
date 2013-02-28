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
package origin

import util.{Events, EventSource, Logger}

trait FactoryComponent {

  protected type Origin[+A] <: amber.Origin[A]
  def origin: OriginFactory

  trait OriginFactory {
    def created: Events[(Origin[_], Manifest[_ ])]
    def create[A: Manifest](name: Property.Name)(read: Origin.Read.Unfiltered[A]): Origin[A]
  }

  object OriginFactory {
    trait Logging extends OriginFactory {

      protected def log: Logger

      abstract override def create[A: Manifest](name: Property.Name)
                                               (read: Origin.Read.Unfiltered[A]) = {
        log.debug("Creating " + name + " origin of type " + manifest[A])
        val result = super.create(name)(read)
        log.info("Created " + name + " origin of type " + manifest[A])
        result
      }
    }
  }
}

object FactoryComponent {
  trait Default extends FactoryComponent with BuilderComponent {

    abstract override protected val builder: OriginBuilder = _builder
    override def origin: OriginFactory = _origin

    trait OriginFactory extends super.OriginFactory {

      override val created = EventSource[(Origin[_], Manifest[_])]()

      override def create[A: Manifest](name: Property.Name)(read: Origin.Read.Unfiltered[A]) =
        builder.build(name, Family.random(), read)
    }

    private object _builder extends OriginBuilder {
      override def build[A: Manifest, B: Origin.Read[A]#apply](name: Property.Name,
                                                               family: Family,
                                                               read: B) = {
        val result = Default.super.builder.build(name, family, read)
        origin.created emit (result, manifest[A])
        result
      }
    }

    private object _origin extends OriginFactory
  }
}
