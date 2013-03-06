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
package simple
package origin

import scala.collection.immutable.HashMap
import scala.collection.JavaConversions._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import amber.util.Logging

trait BuilderComponent extends amber.origin.BuilderComponent {
  this: Logging =>

  override protected type Origin[+A] = simple.Origin[A]
  override protected def builder: super.OriginBuilder = _builder

  protected trait OriginBuilder extends super.OriginBuilder {
    override def build[A: ClassTag : TypeTag](name: Origin.Name,
                                              family: Origin.Family,
                                              _read: OriginBuilder.Read[A]) =
      new Origin(name, family) {
        @transient  private[this] val log = logger.create(s"amber.simple.Origin($name)")
        override def read() =
          for ((value, meta) <- _read(Origin.MetaInfo(HashMap.empty ++ meta))) yield {
            log.debug(s"Read $value from $name")
            (Origin.Value(name, value), meta)
          }
      }
  }

  private object _builder extends OriginBuilder
}
