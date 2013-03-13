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

import scala.language.{dynamics, higherKinds}

import java.util.concurrent.ConcurrentHashMap
import java.util.UUID.randomUUID

import scala.collection.{immutable, Map}
import scala.concurrent.Future

import scalaz.Equal.equalA
import scalaz.Id.Id
import scalaz.OptionT
import scalaz.syntax.equal._

import util.{NotNothing, Path, Type}

sealed trait Origin[X[+_], +A] extends Dynamic with Equals {

  def name: Origin.Name
  def family: Origin.Family

  def selectDynamic(name: Origin.MetaInfo.Name): OptionT[X, Origin.MetaInfo.Value[_]]
  def update[B: Type](name: Origin.MetaInfo.Name, value: B)

  def returns[B: NotNothing : Type]: Boolean
  def read(): OptionT[X, (Origin.Value[A], Origin.MetaInfo)]
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

  trait Local[+A] extends Origin[Id, A]

  trait Remote[+A] extends Origin[Future, A]

  object Local {
    abstract class Default[+A](override val name: Origin.Name, override val family: Origin.Family)
                              (implicit typeA: Type[A]) extends amber.Origin.Local[A] {

      protected val meta = new ConcurrentHashMap[Origin.MetaInfo.Name, Origin.MetaInfo.Value[_]]

      override def selectDynamic(name: Origin.MetaInfo.Name) =
        OptionT[Id, Origin.MetaInfo.Value[_]](Option(meta.get(name)))

      override def update[A: Type](name: Origin.MetaInfo.Name, value: A) {
        meta.put(name, new Origin.MetaInfo.Value(value))
      }

      override def returns[B: NotNothing](implicit typeB: Type[B]) = typeA <:< typeB

      override lazy val hashCode = 41 * (41 + name.hashCode) + family.hashCode

      override def equals(other: Any) = other match {
        case that: Origin.Local[_] =>
          (that canEqual this) &&
          (this.name === that.name) &&
          (this.family === that.family) &&
          that.returns[A]
        case _ => false
      }

      override def canEqual(other: Any) = other.isInstanceOf[Origin.Local[_]]

      override lazy val toString = s"amber.Origin.Local[$typeA]($name)"
    }
  }

  sealed trait MetaInfo extends Dynamic with Serializable {
    def selectDynamic(name: MetaInfo.Name): Option[MetaInfo.Value[_]]
  }

  object MetaInfo {

    type Name = String
    type Value[+A] = util.Value[A]

    def apply(values: Map[Name, Value[_]]): MetaInfo = Default(immutable.HashMap.empty ++ values)
    def compose(first: MetaInfo, second: MetaInfo): MetaInfo = Composition(first, second)

    implicit class Enriched(val meta: MetaInfo) extends AnyVal {
      def :+(other: MetaInfo): MetaInfo = compose(meta, other)
    }

    private case class Default(values: immutable.Map[Name, Value[_]]) extends MetaInfo {
      override def selectDynamic(name: Name) = values.get(name)
    }

    private case class Composition(first: MetaInfo, second: MetaInfo) extends MetaInfo {
      override def selectDynamic(name: Name) =
        first.selectDynamic(name) orElse second.selectDynamic(name)
    }
  }
}
