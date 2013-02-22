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

import scala.collection.immutable.{HashMap, Seq, Stream, Vector}

import scalaz.syntax.equal._

import util.{Filter, Filterable, NotNothing}

trait Client extends origin.FinderComponent {

  def select[A <: AnyRef : NotNothing : Manifest]
      (query: Query): Stream[Property[A]] =
    for {
      origin <- origins.find(query.property).toStream if origin.returns[A]
      property <- origin.asInstanceOf[Origin[A]].apply(query.filter)
    } yield property

  def selectAll[A <: AnyRef : NotNothing : Manifest]
      (query: Query): Stream[A] =
    for {
      origin <- origins.find(query.property).toStream
      if (query.property === origin.name) && origin.returns[A]
      property <- origin.asInstanceOf[Origin[A]].apply(query.filter)
    } yield property.value

  def selectOne[A <: AnyRef : NotNothing : Manifest]
      (query: Query): Option[A] =
    selectAll[A](query).headOption

  def selectAll(definition: Entity.Definition): Stream[Entity.Instance] =
    definition.instances()

  def selectOne(definition: Entity.Definition) =
    selectAll(definition).headOption

  def entity(name: Entity.Name) = Entity.Definition(name, Vector.empty, Filter.tautology)

  object Entity {

    type Name = String

    object Field {

      type Name = String

      private[amber] case class Type[+A <: AnyRef : Manifest]
          (name: Field.Name, query: Query) {

        def values(): Stream[Value[A]] =
          selectAll[A](query) map {Value(name, _)}

        override lazy val toString = name + ": " + manifest[A]
      }

      private[amber] case class Value[+A <: AnyRef : Manifest]
          (name: Field.Name, value: A) {

        def as[B : NotNothing : Manifest]: Option[B] =
          if (manifest[A] <:< manifest[B]) Some(value.asInstanceOf[B])
          else None

        override lazy val toString = name + " = " + value
      }

      private[amber] object Values {
        def apply(types: Seq[Type[_ <: AnyRef]]): Seq[Stream[Value[_ <: AnyRef]]] =
          types map {_.values()}
      }
    }

    case class Definition private[amber]
        (name: Entity.Name, fields: Vector[Field.Type[_ <: AnyRef]], filter: Filter[Instance])
        extends Filterable[Instance, Definition] {

      override def where(filter: Filter[Instance]) = copy(filter = filter)

      def field[A <: AnyRef : NotNothing : Manifest](name: Field.Name) =
        field[A](name, Query(name, Filter.tautology))

      def field[A <: AnyRef : NotNothing : Manifest]
          (name: Field.Name, query: Query) =
        copy(fields = fields :+ Field.Type[A](name, query))

      def instances(): Stream[Instance] = {
        def cartesianProduct(values: Seq[Stream[Field.Value[_ <: AnyRef]]]) =
          values.foldRight(Stream(Stream.empty[Field.Value[_ <: AnyRef]])) {
            for {a <- _; bs <- _} yield a #:: bs
          }

        Instances(
          name,
          cartesianProduct(Field.Values(fields))
        ) filter {filter(_)}
      }

      override lazy val toString = name + fields.mkString("(", ", " ,")")
    }

    case class Instance private[amber]
        (name: Entity.Name, values: Seq[Field.Value[_ <: AnyRef]]) {

      private lazy val properties =
        HashMap.empty ++ (values map {value => (value.name, value)})
      lazy val fields: Seq[Field.Name] = Vector(values map {_.name}: _*)

      def apply[A : NotNothing : Manifest](name: Field.Name): Option[A] =
        properties.get(name) flatMap {_.as[A]}

      override lazy val toString = name + values.mkString("(", ", " ,")")
    }

    private[amber] object Instances {
      def apply(name: Entity.Name, values: Stream[Seq[Field.Value[_ <: AnyRef]]]): Stream[Instance] =
        values map {Instance(name, _)}
    }
  }
}
