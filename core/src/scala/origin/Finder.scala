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
package origin

import scala.language.higherKinds

import scala.collection.immutable.Set
import scala.concurrent.{ExecutionContext, Future}

import scalaz.Id.{id, Id}
import scalaz.Monad

import util.{ConfigurableComponent, MultiTrie, NotNothing, Type}

sealed trait FinderComponent[X[+_]] {

  protected type Origin[+A] <: amber.Origin[A]

  def origins: OriginFinder
  implicit protected def X: Monad[X]

  type Selection = MultiTrie.Selection
  val Selections = MultiTrie.Selections

  trait OriginFinder {
    def find[A: NotNothing : Type](selection: Selection): X[Set[Origin[A]]]
  }
}

object FinderComponent {

  trait Local extends FinderComponent[Id] {
    override protected type Origin[+A] <: Origin.Local[A]
    override implicit protected def X = id
  }

  trait Remote extends FinderComponent[Future] with ConfigurableComponent {

    override protected type Configuration <: Remote.Configuration
    override protected type Origin[+A] <: Origin.Remote[A]

    override implicit protected def X = futureMonad(configuration.context)
  }

  sealed trait Default[X[+_]] extends FinderComponent[X] with BuilderComponent {

    abstract override protected def builder: OriginBuilder = _builder
    override def origins: OriginFinder = _origins

    trait OriginFinder extends super.OriginFinder {

      private val trie = MultiTrie[Origin[_]]()

      override def find[A: NotNothing : Type](selection: Selection) =
        X.point(
          for {origin <- trie.select(selection) if origin.returns[A]}
            yield origin.asInstanceOf[Origin[A]]
        )

      private[Default] def add(origin: Origin[_]) {trie add (origin.name, origin)}
    }

    private object _builder extends OriginBuilder {
      override def build[A: Type](name: Origin.Name, family: Origin.Family)
                                 (read: OriginBuilder.Read[A]) = {
        val result = Default.super.builder.build(name, family)(read)
        origins.add(result)
        result
      }
    }

    private object _origins extends OriginFinder
  }

  sealed trait Delegator[X[+_]] extends FinderComponent[X] {

    protected val finder: FinderComponent[X]

    override protected type Origin[+A] = finder.Origin[A]
    override object origins extends OriginFinder {
      override def find[A: NotNothing : Type](selection: Selection) = finder.origins.find(selection)
    }
  }

  object Local {
    trait Default extends FinderComponent.Local
                  with BuilderComponent.Local
                  with FinderComponent.Default[Id]
  }

  object Remote {

    trait Configuration {
      def context: ExecutionContext
    }

    trait Default extends FinderComponent.Remote
                  with BuilderComponent.Remote
                  with FinderComponent.Default[Future]
                  with ConfigurableComponent {

      override protected type Configuration <: Default.Configuration
    }

    object Default {
      trait Configuration extends BuilderComponent.Remote.Configuration
                          with FinderComponent.Remote.Configuration
    }
  }

  object Delegator {

    trait Local extends Delegator[Id] with FinderComponent.Local {
      override protected val finder: FinderComponent.Local
    }

    trait Remote extends Delegator[Future] with FinderComponent.Remote {
      override protected val finder: FinderComponent.Remote
    }
  }
}
