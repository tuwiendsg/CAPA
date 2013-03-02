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

import scala.reflect.runtime.universe.{typeOf, typeTag, TypeTag}

import scalaz.syntax.equal._

import amber.util.{Filter, NotNothing}

private[simple] abstract class Origin[+A : TypeTag](override val name: Origin.Name,
                                                    override val family: Origin.Family)
    extends amber.Origin[A] with Origin.Meta.Writable.Default {

  override def returns[B: NotNothing : TypeTag] = typeOf[A] <:< typeOf[B]

  override lazy val hashCode =
    41 * (41 * (41 + name.hashCode) + family.hashCode) + typeTag[A].hashCode

  override def equals(other: Any) = other match {
    case that: amber.Origin[_] =>
      (that canEqual this) &&
      (this.name === that.name) &&
      (this.family === that.family) &&
      that.returns[A]
    case _ => false
  }

  override def canEqual(other: Any) = other.isInstanceOf[amber.Origin[_]]

  override lazy val toString = "simple.Origin[" + typeOf[A] + "](" + name + ")"
}
