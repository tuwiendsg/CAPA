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

package amber
package origin

import scala.language.{higherKinds, implicitConversions}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

import scalaz.OptionT
import scalaz.syntax.functor._

import util.{Events, EventSource, Logger, Type}

sealed trait FactoryComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected type Reading[+A]

  def origin: OriginFactory

  trait OriginFactory {
    def created: Events[Origin[_]]
    def create[A: Type](name: Origin.Name)(read: OriginFactory.Read[A]): Origin[A]
  }

  object OriginFactory {

    trait Read[A] extends (() => Reading[A])

    trait Logging extends OriginFactory {

      protected def log: Logger

      abstract override def create[A](name: Origin.Name)(read: OriginFactory.Read[A])
                                     (implicit typeA: Type[A]) = {
        log.debug(s"Creating $name origin of type $typeA")
        val result = super.create(name)(read)
        log.info(s"Created $name origin of type $typeA")
        result
      }
    }
  }
}

object FactoryComponent {

  trait Local extends FactoryComponent {
    override protected type Origin[+A] <: Origin.Local[A]
    override protected type Reading[+A] = Origin.Local.Reading[A]
  }

  trait Remote extends FactoryComponent {
    override protected type Origin[+A] <: Origin.Remote[A]
    override protected type Reading[+A] = Origin.Remote.Reading[A]
  }

  trait Default extends FactoryComponent with BuilderComponent {

    override def origin: OriginFactory

    abstract override protected def builder: OriginBuilder = _builder

    trait OriginFactory extends super.OriginFactory {

      override def created: EventSource[Origin[_]] = _created
      override def create[A: Type](name: Origin.Name)(read: OriginFactory.Read[A]) =
        builder.build(name, Origin.Family.random()) {
          meta => read() map {a => (Origin.Value(name, a), meta)}
        }

      private object _created extends EventSource[Origin[_]]()
    }

    protected trait OriginBuilder extends super.OriginBuilder {

      override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                 (read: OriginBuilder.Read[A]) = {
        val result = Default.super.builder.build(name, family)(read)
        origin.created.emit(result)
        result
      }

      override def map[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B) = {
        val result = Default.super.builder.map(underlying, name)(f)
        origin.created.emit(result)
        result
      }
    }

    private object _builder extends OriginBuilder
  }

  object Local {
    trait Default extends FactoryComponent.Local
                  with BuilderComponent.Local
                  with FactoryComponent.Default {

      override protected type Reading[+A] = Origin.Local.Reading[A]

      override def origin: OriginFactory = _origin

      implicit protected class ReadValue[A](f: () => A) extends OriginFactory.Read[A] {
        override def apply() = Option(f())
      }

      implicit protected class ReadOption[A](f: () => Option[A]) extends OriginFactory.Read[A] {
        override def apply() = f()
      }

      private object _origin extends OriginFactory
    }
  }

  object Remote {

    trait Default extends FactoryComponent.Remote
                  with BuilderComponent.Remote
                  with FactoryComponent.Default {

      override protected type Reading[+A] = Origin.Remote.Reading[A]

      override def origin: OriginFactory = _origin
      implicit private def context = configuration.context

      implicit protected class ReadValue[A](f: () => Future[A]) extends OriginFactory.Read[A] {
        override def apply() = OptionT(f() map {Option(_)})
      }

      implicit protected class ReadOption[A](f: () => Future[Option[A]])
          extends OriginFactory.Read[A] {
        override def apply() = OptionT(f())
      }

      private object _origin extends OriginFactory
    }
  }
}
