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

import scala.language.{higherKinds, implicitConversions}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, typeTag, TypeTag}

import util.{Events, EventSource, Logger}

trait FactoryComponent {

  protected type Origin[+A] <: amber.Origin[A]
  def origin: OriginFactory

  trait OriginFactory {
    def created: Events[Origin[_]]
    def create[A: ClassTag : TypeTag](name: Origin.Name)(read: OriginFactory.Read[A]): Origin[A]
  }

  object OriginFactory {

    sealed trait Read[A] extends (() => Option[A])

    implicit def fromValue[A](f: () => A): Read[A] = new Read[A] {
      override def apply() = Option(f())
    }

    implicit def fromOption[A](f: () => Option[A]): Read[A] = new Read[A] {
      override def apply() = f()
    }

    implicit def fromFuture[A](f: () => Future[A])(implicit timeout: FiniteDuration): Read[A] =
      new Read[A] {
        override def apply() = Option(Await.result(f(), timeout))
      }

    trait Logging extends OriginFactory {

      protected def log: Logger

      abstract override def create[A: ClassTag : TypeTag](name: Origin.Name)
                                                         (read: OriginFactory.Read[A]) = {
        log.debug(s"Creating $name origin of type ${typeOf[A]}")
        val result = super.create(name)(read)
        log.info(s"Created $name origin of type ${typeOf[A]}")
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

      override val created = EventSource[Origin[_]]()

      override def create[A: ClassTag : TypeTag](name: Origin.Name)(read: OriginFactory.Read[A]) =
        builder.build(name, Origin.Family.random()) {meta => read() map {(_, meta)}}
    }

    private object _builder extends OriginBuilder {
      override def build[A: ClassTag : TypeTag](name: Origin.Name, family: Origin.Family)
                                               (read: OriginBuilder.Read[A]) = {
        val result = Default.super.builder.build(name, family)(read)
        origin.created.emit(result)
        result
      }
    }

    private object _origin extends OriginFactory
  }
}
