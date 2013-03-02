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

import scala.language.dynamics

import java.util.concurrent.ConcurrentHashMap
import java.util.UUID.randomUUID

import scala.reflect.runtime.universe.TypeTag

import scalaz.Equal.equalA

import util.{NotNothing, Path}

trait Origin[+A] extends Equals with Origin.Meta.Writable {
  def read(): Option[(Origin.Value[A], Origin.Meta.Readable)]
  def returns[B: NotNothing : TypeTag]: Boolean
}

object Origin {

  type Name = Path
  type Value[+A] = util.Value.Named[Name, A]

  val Value = util.Value.Named

  case class Family private(private val id: String)

  object Family {
    def random() = Family(randomUUID().toString)
    implicit val hasEqual = equalA[Family]
  }

  object Meta {

    trait Readable extends Dynamic {
      def name: Origin.Name
      def family: Origin.Family
      def selectDynamic(name: String): Option[MetaInfo.Value[_]]
    }

    trait Writable extends Readable {
      def update[A: TypeTag](name: MetaInfo.Name, value: A)
    }

    object Writable {
      trait Default extends Writable {

        private val values = new ConcurrentHashMap[MetaInfo.Name, MetaInfo.Value[_]]

        override def selectDynamic(name: MetaInfo.Name) = Option(values.get(name))

        override def update[A: TypeTag](name: MetaInfo.Name, value: A) {
          values.put(name, new MetaInfo.Value(value))
        }
      }
    }
  }

  object MetaInfo {
    type Name = String
    type Value[+A] = util.Value[A]
  }
}
