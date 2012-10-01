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

import util.{Filter, Filterable, NotNothing, Path}

case class Property[+A: Manifest](name: Property.Name, value: A) {

  def as[B: NotNothing : Manifest]: Option[B] =
    if (manifest[A] <:< manifest[B]) Some(value.asInstanceOf[B]) else None

  override lazy val toString = name + " = " + value
}

object Property {
  type Name = Path
}
