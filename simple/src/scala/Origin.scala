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

import scalaz.syntax.equal._

import amber.Origin.Meta
import amber.util.{Filter, Logger, NotNothing}

private[simple] abstract class Origin[+A <: AnyRef : Manifest]
    (override val name: Property.Name, override val family: Family)
    (log: Logger) extends amber.Origin[A] {

  protected def read(filter: Filter[Meta.Readable]): Option[A]

  override def apply(filter: Filter[Meta.Readable]) =
    for (value <- read(filter)) yield {
      log.debug("Read " + value + " for property " + name)
      Property(name, value)
    }

  override def returns[B <: AnyRef : NotNothing : Manifest] = manifest[A] <:< manifest[B]

  override lazy val hashCode =
    41 * (41 * (41 + name.hashCode) + family.hashCode) + manifest[A].hashCode

  override def equals(other: Any) = other match {
    case that: amber.Origin[_] =>
      (that canEqual this) &&
      (this.name === that.name) &&
      (this.family === that.family) &&
      that.returns[A]
    case _ => false
  }

  override def canEqual(other: Any) = other.isInstanceOf[amber.Origin[_]]

  override lazy val toString = "simple.Origin[" + manifest[A] + "](" + name + ")"
}
