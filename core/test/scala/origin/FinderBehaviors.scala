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
import scala.concurrent.{Await, Future}

import scalaz.Comonad
import scalaz.Id.{id, Id}

import util.{NotNothing, Type}

trait FinderBehaviors[X[+_]] {
  this: Spec with FinderComponent[X] =>

  def Y: Comonad[X]
  def fixture: Fixture

  class A
  class U extends A
  class B

  trait Fixture {
    def create[A: NotNothing : Type](name: Origin.Name): Origin[A]
  }

  def anOriginFinder {
    "find an origin" when {
      "type is the same" in {
        val name = random[Origin.Name]
        val origin = fixture.create[A](name)

        Y.copoint(origins.find[A](Selections.exact(name))) should contain(origin)
      }

      "type is a super type" in {
        val name = random[Origin.Name]
        val origin = fixture.create[U](name)

        val result = Y.copoint(origins.find[A](Selections.exact(name))) map {
          _.asInstanceOf[Origin[Any]]
        }
        result should contain(origin.asInstanceOf[Origin[Any]])
      }
    }

    "not find an origin" when {
      "type is a sub type" in {
        val name = random[Origin.Name]
        val origin = fixture.create[A](name)

        Y.copoint(origins.find[U](Selections.exact(name))) should not(contain(origin))
      }

      "type is a different type" in {
        val name = random[Origin.Name]
        val origin = fixture.create[A](name)

        val result = Y.copoint(origins.find[B](Selections.exact(name))) map {
          _.asInstanceOf[Origin[Any]]
        }
        result should not(contain(origin.asInstanceOf[Origin[Any]]))
      }
    }
  }
}

object FinderBehaviors {

  trait Local extends FinderBehaviors[Id] {
    this: Spec with FinderComponent.Local =>

    override def Y = id
  }

  trait Remote extends FinderBehaviors[Future] {
    this: Spec with FinderComponent.Remote =>

    override object Y extends Comonad[Future] {
      override def copoint[A](fa: Future[A]): A = Await.result(fa, timeout)
      override def cobind[A, B](fa: Future[A])(f: Future[A] => B) = ???
      override def cojoin[A](fa: Future[A]): Future[Future[A]] = ???
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = ???
    }
  }
}
