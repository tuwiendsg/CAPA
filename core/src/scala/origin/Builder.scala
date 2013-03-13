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

import scala.collection.JavaConversions._

import scalaz.Id.Id
import scalaz.OptionT
import scalaz.syntax.applicative._

import util.Type

trait BuilderComponent {

  protected type Origin[+A] <: Origin.Local[A]
  protected def builder: OriginBuilder

  protected trait OriginBuilder {
    def build[A: Type](name: Origin.Name, family: Origin.Family)
                      (read: OriginBuilder.Read[A]): Origin[A]
  }

  protected object OriginBuilder {
    type Read[+A] = (Origin.MetaInfo) => Option[(A, Origin.MetaInfo)]
  }
}

object BuilderComponent {

  trait Default extends BuilderComponent {

    override protected type Origin[+A] = Origin.Local.Default[A]
    override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                 (_read: OriginBuilder.Read[A]) =
        new Origin(name, family) {
          override def read() = OptionT((
            for {(value, meta) <- _read(Origin.MetaInfo(meta))}
              yield (Origin.Value(name, value), meta)
          ).point[Id])
        }
    }
  }

  trait Logging extends BuilderComponent {
    this: util.Logging =>

    abstract override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      override def build[A](name: Origin.Name, family: Origin.Family)
                           (read: OriginBuilder.Read[A])
                           (implicit typeA: Type[A]) = {
        val log = logger.create(s"Origin.Local[$typeA]($name)")
        Logging.super.builder.build(name, family)(read andThen {
          case result@Some((value, _)) =>
            log.debug(s"Read $value from $name")
            result
          case other => other
        })
      }
    }
  }
}
