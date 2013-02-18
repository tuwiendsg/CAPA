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

import scala.language.{dynamics, implicitConversions}

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.{HashMap, Seq, Set, Stream, Vector}
import scala.collection.JavaConversions._
import scala.reflect.runtime.universe.{typeOf, TypeTag}

import util.{Filter, Filterable, NotNothing}

trait Client extends origin.FinderComponent {

  case class Query(selection: Selection, filter: Filter[Origin.Meta.Readable])
      extends Filterable[Origin.Meta.Readable, Query] {
    override def where(filter: Filter[Origin.Meta.Readable]): Query = copy(filter = filter)
  }

  implicit def selectionToQuery(selection: Selection): Query = Query(selection, Filter.tautology)

  def read[A: NotNothing : TypeTag](query: Query): Stream[Origin.Value[A]] =
    for {
      origin <- origins.find(query.selection).toStream if origin.returns[A]
      value <- origin.asInstanceOf[Origin[A]].read(query.filter)
    } yield value

  def readAll[A: NotNothing : TypeTag](query: Query): Stream[A] = read[A](query) map {_.value}
  def readOne[A: NotNothing : TypeTag](query: Query): Option[A] = readAll[A](query).headOption
  def readAll(definition: Entity.Definition): Stream[Entity.Instance] = definition.instances()
  def readOne(definition: Entity.Definition): Option[Entity.Instance] =
    readAll(definition).headOption

  def entity(name: Entity.Name) = new Entity.Definition.Builder(name)

  object Entity {

    type Name = String

    object Field {

      type Name = String

      private[amber] case class Type[+A: NotNothing : TypeTag](name: Field.Name, query: Query) {
        def values(): Stream[Value[A]] = readAll[A](query) map {Value(name, _)}
        override lazy val toString = s"$name: ${typeOf[A]}"
      }

      private[amber] type Value[+A] = util.Value.Named[Field.Name, A]
      private[amber] val Value = util.Value.Named

      private[amber] object Values {
        def apply(types: Set[Type[_]]): Seq[Stream[Value[_]]] = types.to[Vector] map {_.values()}
      }
    }

    case class Definition private[amber](name: Entity.Name,
                                         fields: Set[Field.Type[_]],
                                         filter: Filter[Instance]) {

      def instances(): Stream[Instance] = {
        def cartesianProduct(values: Seq[Stream[Field.Value[_]]]) =
          values.foldRight(Stream(Vector.empty[Field.Value[_]])) {
            for {a <- _; bs <- _} yield bs :+ a
          }

        Instances(name, cartesianProduct(Field.Values(fields))) filter {filter(_)}
      }

      override lazy val toString = name + fields.mkString("(", ", " ,")")
    }

    object Definition {
      class Builder(val name: Entity.Name) extends Filterable[Instance, Unit] {

        private var filter: Filter[Instance] = Filter.tautology
        private val fields = new ConcurrentHashMap[Field.Name, Field.Type[_]]

        override def where(filter: Filter[Instance]) {this.filter = filter}

        def has[A: NotNothing : TypeTag] = new Has[A]

        class Has[A: NotNothing : TypeTag] extends Dynamic {
          def updateDynamic(name: String)(query: Query) {
            fields put (name, Field.Type[A](name, query))
          }
        }

        def build(): Definition = Definition(name, fields.values.to[Set], filter)
      }
    }

    case class Instance private[amber](name: Entity.Name,
                                       values: Seq[Field.Value[_]]) extends Dynamic {

      private lazy val properties = HashMap.empty ++ (values map {v => (v.name, v)})
      lazy val fields: Set[Field.Name] = (values map {_.name}).to[Set]

      def selectDynamic(name: String): Option[Field.Value[_]] = properties.get(name)

      override lazy val toString = name + values.mkString("(", ", " ,")")
    }

    private[amber] object Instances {
      def apply(name: Entity.Name, values: Stream[Seq[Field.Value[_]]]): Stream[Instance] =
        values map {Instance(name, _)}
    }
  }
}
