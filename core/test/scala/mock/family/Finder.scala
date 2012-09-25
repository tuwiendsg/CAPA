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
package mock.family

import scala.collection.immutable.Vector

import scalaz.syntax.equal._

import org.scalatest.{BeforeAndAfterEach, Suite}
import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import org.scalatest.mock.MockitoSugar.mock

import amber.util.Mocking
import amber.util.NotNothing.notNothing

trait FinderComponent extends amber.family.FinderComponent
                      with Mocking {

  var families: FamilyFinder = _

  object FamilyFinder {

    var added: Vector[Origin[_ <: AnyRef]] = _

    def add(origin: Origin[_ <: AnyRef]) {
      added = added :+ origin
    }

    def reset() {
      added = Vector.empty
      families = mock[FamilyFinder]("mock.FamilyFinder")

      when(families.find(anything())) thenAnswer {
        args: Array[AnyRef] =>
          val family = args(0).asInstanceOf[Family]
          added filter {family === _.family}
      }

      when(families.all()) thenAnswer {
        _: Array[AnyRef] => added
      }
    }
  }
}

object FinderComponent {

  trait WithSuite extends FinderComponent with BeforeAndAfterEach {
    this: Suite =>

    override def beforeEach() {
      FamilyFinder.reset()

      super.beforeEach()
    }
  }

  trait WithBuilder extends WithSuite with BeforeAndAfterEach {
    this: amber.mock.origin.BuilderComponent with Suite =>

    override def beforeEach() {
      super.beforeEach()

      when(build(anything(), anything(), anything())) thenAnswer {
        _: Array[AnyRef] => FamilyFinder.add(built.last)
      }
    }
  }
}
