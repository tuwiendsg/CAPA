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

import scala.language.higherKinds

import scala.concurrent.ExecutionContext

import scalaz.Monad
import scalaz.OptionT.optionTMonadPlus
import scalaz.std.option.optionInstance
import scalaz.syntax.monad._

import util.{ConfigurableComponent, Type}

trait BuilderComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected type Reading[+A]

  implicit protected def Reading: Monad[Reading]

  protected def builder: OriginBuilder
  protected trait OriginBuilder {
    def build[A: Type](name: Origin.Name, family: Origin.Family)
                      (read: OriginBuilder.Read[A]): Origin[A]
  }

  protected object OriginBuilder {
    type Read[+A] = (Origin.MetaInfo) => Reading[(Origin.Value[A], Origin.MetaInfo)]
  }
}

object BuilderComponent {

  trait Local extends BuilderComponent {

    override protected type Origin[+A] <: Origin.Local[A]

    override protected type Reading[+A] = Origin.Local.Reading[A]
    override implicit protected def Reading = optionInstance
  }

  trait Remote extends BuilderComponent with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration
    override protected type Origin[+A] <: Origin.Remote[A]

    override protected type Reading[+A] = Origin.Remote.Reading[A]
    override implicit protected def Reading = optionTMonadPlus(futureMonad(configuration.context))
  }

  sealed trait Logging extends BuilderComponent {
    this: util.Logging =>

    private[BuilderComponent] val loggerPrefix = "Origin"
    abstract override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      override def build[A](name: Origin.Name, family: Origin.Family)
                           (read: OriginBuilder.Read[A])
                           (implicit typeA: Type[A]) = {
        val log = logger.create(s"$loggerPrefix[$typeA]($name)")
        Logging.super.builder.build(name, family)(read andThen {
          result =>
            result.map {case (value, _) => log.debug(s"Read $value")}
            result
        })
      }
    }
  }

  object Local {
    trait Default extends Local {

      override protected type Origin[+A] = Origin.Local.Default[A]
      override protected def builder: OriginBuilder = _builder

      private object _builder extends OriginBuilder {
        override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                   (_read: OriginBuilder.Read[A]) =
          new Origin(name, family) {
            override def read() = _read(meta)
          }
      }
    }
  }

  object Remote {
    trait Configuration {
      def context: ExecutionContext
    }
  }

  object Logging {

    trait Local extends BuilderComponent.Local with Logging {
      this: util.Logging =>

      override private[BuilderComponent] val loggerPrefix = "Origin.Local"
    }

    trait Remote extends BuilderComponent.Remote with Logging {
      this: util.Logging =>

      override private[BuilderComponent] val loggerPrefix = "Origin.Remote"
    }
  }
}
