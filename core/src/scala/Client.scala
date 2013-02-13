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

import scala.collection.immutable.{HashMap, Seq, Set, Stream, Vector}

import util.{Filter, Filterable, NotNothing}

trait Client extends origin.FinderComponent {

  case class Query(selection: Selection, filter: Filter[Origin.Meta.Readable])
      extends Filterable[Origin.Meta.Readable, Query] {
    override def where(filter: Filter[Origin.Meta.Readable]): Query = copy(filter = filter)
  }

  implicit def selectionToQuery(selection: Selection): Query = Query(selection, Filter.tautology)

  def read[A: NotNothing : Manifest](query: Query): Stream[Origin.Value[A]] =
    for {
      origin <- origins.find(query.selection).toStream if origin.returns[A]
      value <- origin.asInstanceOf[Origin[A]].read(query.filter)
    } yield value

  def readAll[A: NotNothing : Manifest](query: Query): Stream[A] = read[A](query) map {_.value}
  def readOne[A: NotNothing : Manifest](query: Query): Option[A] = readAll[A](query).headOption
  def readAll(definition: Entity.Definition): Stream[Entity.Instance] = definition.instances()
  def readOne(definition: Entity.Definition): Option[Entity.Instance] =
    readAll(definition).headOption

  def entity(name: Entity.Name) = Entity.Definition(name, HashMap.empty, Filter.tautology)

  object Entity {

    type Name = String

    object Field {

      type Name = String

      private[amber] case class Type[+A: NotNothing : Manifest](name: Field.Name, query: Query) {
        def values(): Stream[Value[A]] = readAll[A](query) map {Value(name, _)}
        override lazy val toString = name + ": " + manifest[A]
      }

      private[amber] type Value[+A] = util.Value.Named[Field.Name, A]
      private[amber] val Value = util.Value.Named

      private[amber] object Values {
        def apply(types: Set[Type[_]]): Seq[Stream[Value[_]]] =
          (Vector.empty ++ types) map {_.values()}
      }
    }

    case class Definition private[amber](name: Entity.Name,
                                         fields: HashMap[Field.Name, Field.Type[_]],
                                         filter: Filter[Instance])
        extends Filterable[Instance, Definition] {

      override def where(filter: Filter[Instance]) = copy(filter = filter)

      def field[A: NotNothing : Manifest](name: Field.Name, query: Query): Definition =
        copy(fields = fields.updated(name, Field.Type[A](name, query)))

      def instances(): Stream[Instance] = {
        def cartesianProduct(values: Seq[Stream[Field.Value[_]]]) =
          values.foldRight(Stream(Vector.empty[Field.Value[_]])) {
            for {a <- _; bs <- _} yield bs :+ a
          }

        Instances(name, cartesianProduct(Field.Values(Set.empty ++ fields.values))) filter {filter(_)}
      }

      override lazy val toString = name + fields.mkString("(", ", " ,")")
    }

    case class Instance private[amber](name: Entity.Name, values: Seq[Field.Value[_]]) {

      private lazy val properties = HashMap.empty ++ (values map {value => (value.name, value)})
      lazy val fields: Set[Field.Name] = Set.empty ++ (values map {_.name})

      def apply[A: NotNothing : Manifest](name: Field.Name): Option[A] =
        properties.get(name) flatMap {_.as[A]}

      override lazy val toString = name + values.mkString("(", ", " ,")")
    }

    private[amber] object Instances {
      def apply(name: Entity.Name, values: Stream[Seq[Field.Value[_]]]): Stream[Instance] =
        values map {Instance(name, _)}
    }
  }
}
