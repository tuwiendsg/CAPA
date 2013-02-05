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

import scalaz.Equal.equalA
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.apply._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._

import util.{Filter, Filterable, NotNothing}

case class Property[+A <: AnyRef : Manifest](name: Property.Name, value: A) {

  def as[B: NotNothing : Manifest]: Option[B] =
    if (manifest[A] <:< manifest[B]) Some(value.asInstanceOf[B]) else None

  override lazy val toString = name + " = " + value
}

object Property {

  case class Name(private[amber] val property: String, private[amber] val child: Option[Name] = None)
      extends Filterable[Origin.Meta.Readable, Query] {

    def /:(parent: String) = new Name(parent, Some(this))

    def /(part: String): Name = copy(child = Some((child map {_ / part}) | Name(part)))

    def >:>(that: Name): Boolean =
      (this.property === that.property) &&
      ((!this.child.isDefined) ||
      (((this.child |@| that.child) {_ >:> _}) | false))

    override def where(filter: Filter[Origin.Meta.Readable]) = Query(this, filter)

    override lazy val toString: String = property + ((child map {"/" + _.toString}) | "")
  }

  object Name {

    def apply(property: String): Name = {
      val parts = property.split("/") dropWhile {_.isEmpty}
      parts.init.foldRight(new Name(parts.last)) {
        case (parent, child) => new Name(parent, Some(child))
      }
    }

    implicit val hasEqual = equalA[Name]

    implicit def fromString(property: String): Name = Name(property)
  }
}
