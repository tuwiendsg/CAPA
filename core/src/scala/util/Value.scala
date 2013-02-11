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
package util

import scala.reflect.runtime.universe.{typeOf, TypeTag}

class Value[+A: TypeTag](value: A) {
  def as[B: NotNothing : TypeTag]: Option[B] =
    if (typeOf[A] <:< typeOf[B]) Some(value.asInstanceOf[B]) else None
}

object Value {
  case class Named[A, +B: TypeTag](name: A, value: B) extends Value[B](value) {
    override lazy val toString = s"$name = $value"
  }
}
