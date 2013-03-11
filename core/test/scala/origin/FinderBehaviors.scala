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

import scala.concurrent.Future

import scalaz.Comonad
import scalaz.Id.{id, Id}

import util.FutureComonad

sealed trait FinderBehaviors[X[+_]] {
  this: Spec with FinderComponent[X] =>

  def X: Comonad[X]
  def fixture: Fixture

  trait Fixture {
    def create(name: Origin.Name): Origin[_]
  }

  object aFinder {
    def forOrigins() {
      "find an origin" in {
        val name = random[Origin.Name]
        val origin = fixture.create(name)

        X.copoint(origins.find(Selections.exact(name))) should contain(origin)
      }
    }
  }
}

object FinderBehaviors {

  trait Local extends FinderBehaviors[Id] {
    this: Spec with FinderComponent.Local =>

    override val X = id
  }

  trait Remote extends FinderBehaviors[Future] with FutureComonad {
    this: Spec with FinderComponent.Remote =>

    override val X = implicitly[Comonad[Future]]
  }
}
