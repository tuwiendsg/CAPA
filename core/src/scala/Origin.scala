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
import java.util.UUID.randomUUID

import scalaz.Equal.equalA

import util.{Filter, NotNothing, Path, Union}

trait Origin[+A] extends Equals {

  def name: Origin.Name
  def family: Origin.Family
  def apply(filter: Filter[Origin.Meta.Readable]): Option[Property[A]]
  def returns[B: NotNothing : Manifest]: Boolean

  val meta = Origin.Meta.Writable()
}

object Origin {

  type Name = Path
  type Read[A] = Union.of[Read.Unfiltered[A]]#and[Read.Filtered[A]]

  case class Family private(private val id: String)

  object Family {
    def random() = Family(randomUUID().toString)
    implicit val hasEqual = equalA[Family]
  }

  object Read {
    type Unfiltered[+A] = () => Option[A]
    type Filtered[+A] = Filter[Meta.Readable] => Option[A]
  }

  object Meta {

    trait Readable {
      def apply[A: NotNothing : Manifest](name: MetaInfo.Name): Option[A]
    }

    trait Writable extends Readable {
      def update[A: Manifest](name: MetaInfo.Name, value: A)
    }

    object Writable {
      def apply(): Writable = new Writable {

        private val values = new ConcurrentHashMap[MetaInfo.Name, MetaInfo.Value[_]]

        override def apply[A: NotNothing : Manifest](name: MetaInfo.Name) =
          Option(values.get(name)) flatMap {_.as[A]}

        override def update[A: Manifest](name: MetaInfo.Name, value: A) {
          values.put(name, MetaInfo.Value(name, value))
        }
      }
    }
  }

  object MetaInfo {

    type Name = String

    private[amber] case class Value[+A: Manifest](name: MetaInfo.Name, private val value: A) {
      def as[B: NotNothing : Manifest]: Option[B] =
        if (manifest[A] <:< manifest[B]) Some(value.asInstanceOf[B]) else None
    }
  }
}
