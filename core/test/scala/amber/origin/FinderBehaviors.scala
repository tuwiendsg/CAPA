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

package amber
package origin

import scala.language.higherKinds

import scala.concurrent.{Await, Future}

import scalaz.Id.Id

import amber.util.{NotNothing, Type}

trait FinderBehaviors[X[+_]] {
  this: Spec with FinderComponent[X] =>

  def copoint[A](value: X[A]): A
  def build[A: NotNothing : Type](name: Origin.Name): Origin[A]

  class A
  class U extends A
  class B

  trait Fixture {
    val name = random[Origin.Name]
  }

  def anOriginFinder() {
    "find an origin" when {
      "type is the same" in {
        new Fixture {
          val origin = build[A](name)

          copoint(origins.find[A](Selections.exact(name))) should contain(origin)
        }
      }

      "type is a super type" in {
        new Fixture {
          val origin = build[U](name)

          val result = copoint(origins.find[A](Selections.exact(name)))
          result should contain(origin.asInstanceOf[Any])
        }
      }
    }

    "not find an origin" when {
      "type is a sub type" in {
        new Fixture {
          val origin = build[A](name)

          copoint(origins.find[U](Selections.exact(name))) should not(contain(origin))
        }
      }

      "type is a different type" in {
        new Fixture {
          val origin = build[A](name)

          val result = copoint(origins.find[B](Selections.exact(name)))
          result should not(contain(origin.asInstanceOf[Any]))
        }
      }
    }
  }
}

object FinderBehaviors {

  trait Local extends FinderBehaviors[Id] {
    this: Spec with FinderComponent.Local =>

    override def copoint[A](value: Id[A]) = value
  }

  trait Remote extends FinderBehaviors[Future] {
    this: Spec with FinderComponent.Remote =>

    override def copoint[A](future: Future[A]) = Await.result(future, timeout)
  }
}
