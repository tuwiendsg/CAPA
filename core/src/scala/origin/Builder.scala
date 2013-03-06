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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait BuilderComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected def builder: OriginBuilder

  protected trait OriginBuilder {
    def build[A: ClassTag : TypeTag](name: Origin.Name,
                                     family: Origin.Family,
                                     read: OriginBuilder.Read[A]): Origin[A]
  }

  protected object OriginBuilder {
    type Read[+A] = (Origin.MetaInfo) => Option[(A, Origin.MetaInfo)]
  }
}
