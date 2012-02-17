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

import java.util.concurrent.ConcurrentHashMap

import util.{Filter, NotNothing, Union}

trait Origin[+A <: AnyRef] {

  def name: Property.Name
  def family: Family
  def apply(filter: Filter[Origin.Meta.Readable]): Option[Property[A]]
  def returns[B <: AnyRef : NotNothing : Manifest]: Boolean

  val meta = Origin.Meta.Writable()
}

object Origin {

  type Read[A <: AnyRef] = Union.of[Read.Unfiltered[A]]#and[Read.Filtered[A]]

  object Read {
    type Unfiltered[+A <: AnyRef] = () => Option[A]
    type Filtered[+A <: AnyRef] = Filter[Meta.Readable] => Option[A]
  }

  object Meta {

    trait Readable {
      def apply[A <: AnyRef : NotNothing : Manifest](name: MetaInfo.Name): Option[A]
    }

    trait Writable extends Readable {
      def update[A <: AnyRef : Manifest](name: MetaInfo.Name, value: A)
    }

    object Writable {
      def apply(): Writable = new Writable {

        private val values = new ConcurrentHashMap[MetaInfo.Name, MetaInfo.Value[_ <: AnyRef]]

        override def apply[A <: AnyRef : NotNothing : Manifest](name: MetaInfo.Name) =
          Option(values.get(name)) flatMap {_.as[A]}

        override def update[A <: AnyRef : Manifest](name: MetaInfo.Name, value: A) {
          values.put(name, MetaInfo.Value(name, value))
        }
      }
    }
  }

  object MetaInfo {

    type Name = String

    private[amber] case class Value[+A <: AnyRef : Manifest]
        (name: MetaInfo.Name, private val value: A) {
      def as[B <: AnyRef : NotNothing : Manifest]: Option[B] =
        if (manifest[A] <:< manifest[B]) Some(value.asInstanceOf[B])
        else None
    }
  }
}
