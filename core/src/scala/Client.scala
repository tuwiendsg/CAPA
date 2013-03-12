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

import scala.language.{dynamics, higherKinds, implicitConversions}

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.{HashMap, Seq, Set, Stream, Vector}
import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Future}

import scalaz.Applicative
import scalaz.Id.{id, Id}
import scalaz.std.vector._
import scalaz.syntax.applicative._

import util.{ConfigurableComponent, Filter, Filterable, NotNothing, Type}

sealed trait Client[X[+_]] {
  this: origin.FinderComponent[X] =>

  implicit def X: Applicative[X]
  def read[A: NotNothing : Type](query: Query): X[Seq[Origin.Value[A]]]

  class Query(val selection: Selection, val filter: Filter[Origin.MetaInfo])
      extends Filterable[Origin.MetaInfo, Query] {
    override def where(filter: Filter[Origin.MetaInfo]): Query = new Query(selection, filter)
  }

  implicit def selectionToQuery(selection: Selection): Query =
    new Query(selection, Filter.tautology)

  def readAll[A: NotNothing : Type](query: Query): X[Seq[A]] = read[A](query) map {_ map {_.value}}
  def readOne[A: NotNothing : Type](query: Query): X[Option[A]] =
    readAll[A](query) map {_.headOption}

  def readAll(definition: Entity.Definition): X[Seq[Entity.Instance]] = definition.instances()
  def readOne(definition: Entity.Definition): X[Option[Entity.Instance]] =
    readAll(definition) map {_.headOption}

  def entity(name: Entity.Name) = new Entity.Definition.Builder(name)

  object Entity {

    type Name = String

    object Field {

      type Name = String
      sealed trait Read[+A] extends ((Field.Name) => X[Seq[Value[A]]])

      implicit def readFromOption[A: util.Type](f: () => X[Option[A]]): Read[A] = new Read[A] {
        override def apply(name: Field.Name) = f() map {_.to[Stream] map (Value(name, _))}
      }
      implicit def readFromSeq[A: util.Type](f: () => X[Seq[A]]): Read[A] = new Read[A] {
        override def apply(name: Field.Name) = f() map {_.to[Stream] map (Value(name, _))}
      }

      private[Entity] class Type[+A](val name: Field.Name)(read: Read[A]) {
        def values(): X[Seq[Value[A]]] = read(name)
      }

      private[Entity] type Value[+A] = util.Value.Named[Field.Name, A]
      private[Entity] val Value = util.Value.Named

      private[Entity] object Values {
        def apply(types: Set[Type[_]]): X[Seq[Seq[Value[_]]]] =
          X.sequence(types.to[Vector] map {_.values()})
      }
    }

    class Definition private[Entity](val name: Entity.Name, val filter: Filter[Instance])
                                    (types: Set[Field.Type[_]]) {

      lazy val fields: Set[Field.Name] = types map {_.name}

      def instances(): X[Seq[Instance]] = {
        def cartesianProduct(values: X[Seq[Seq[Field.Value[_]]]]) =
          values map {_.foldRight(Seq(Vector.empty[Field.Value[_]])) {
            for {a <- _; bs <- _} yield bs :+ a
          }}

        Instances(name, cartesianProduct(Field.Values(types))) map {_ filter {filter(_)}}
      }

      override lazy val toString = name + types.mkString("(", ", " ,")")
    }

    object Definition {
      class Builder(val name: Entity.Name) extends Filterable[Instance, Unit] with Dynamic {

        private var filter: Filter[Instance] = Filter.tautology
        private val fields = new ConcurrentHashMap[Field.Name, Field.Type[_]]

        override def where(filter: Filter[Instance]) {this.filter = filter}

        def updateDynamic[A](name: String)(read: Field.Read[A]) {
          fields put (name, new Field.Type[A](name)(read))
        }

        def build(): Definition = new Definition(name, filter)(fields.values.to[Set])
      }
    }

    class Instance private[Entity](val name: Entity.Name)
                                  (values: Seq[Field.Value[_]]) extends Dynamic {

      private lazy val properties = HashMap.empty ++ (values map {v => (v.name, v)})
      lazy val fields: Set[Field.Name] = (values map {_.name}).to[Set]

      def selectDynamic(name: String): Option[Field.Value[_]] = properties.get(name)

      override lazy val toString = name + values.mkString("(", ", " ,")")
    }

    private[Entity] object Instances {
      def apply(name: Entity.Name, values: X[Seq[Seq[Field.Value[_]]]]): X[Seq[Instance]] =
        values map {_ map {new Instance(name)(_)}}
    }
  }
}

object Client {

  trait Local extends Client[Id] with origin.FinderComponent.Local {

    override implicit val X = id

    override def read[A: NotNothing : Type](query: Query) =
      for {
        origin <- origins.find(query.selection).to[Stream] if origin.returns[A]
        (value, meta) <- origin.asInstanceOf[Origin[A]].read().run if query.filter(meta)
      } yield value
  }

  trait Remote extends Client[Future]
               with origin.FinderComponent.Remote
               with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration
    implicit private def context: ExecutionContext = configuration.context

    override implicit val X: Applicative[Future] = new Applicative[Future] {
      override def point[A](a: => A) = Future.successful(a)
      override def map[A, B](future: Future[A])(f: A => B) = future.map(f)
      override def ap[A, B](fa: => Future[A])(ff: => Future[A => B]) =
        fa flatMap {a => ff map {_(a)}}
    }

    override def read[A: NotNothing : Type](query: Query) =
      origins.find(query.selection) map {
        _.to[Vector] map {_.read().run}
      } flatMap {X.sequence(_) map {
        for {
          reading <- _
          (value, meta) <- reading if query.filter(meta)
          a <- value.as[A]
        } yield Origin.Value(value.name, a)
      }}
  }

  object Remote {
    trait Configuration {
      def context: ExecutionContext
    }
  }
}
