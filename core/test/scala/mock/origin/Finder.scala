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
package mock.origin

import scala.collection.immutable.Set

import org.scalatest.{BeforeAndAfterEach, Suite}
import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import org.scalatest.mock.MockitoSugar.mock

import util.Mocking

trait FinderComponent extends amber.origin.FinderComponent
                      with Mocking {

  var origins: OriginFinder = _

  object OriginFinder {

    var added: Set[Origin[_]] = _

    def add(origin: Origin[_]) {added = added + origin}

    def reset() {
      added = Set.empty
      origins = mock[OriginFinder]("mock.OriginFinder")

      when(origins.find(anything())) thenAnswer {
        args: Array[AnyRef] =>
          val name = args(0).asInstanceOf[Property.Name]
          added filter {name >:> _.name}
      }

      when(origins.all()) thenAnswer {_: Array[AnyRef] => added}
    }
  }
}

object FinderComponent {

  trait WithSuite extends FinderComponent with BeforeAndAfterEach {
    this: Suite =>

    override def beforeEach() {
      OriginFinder.reset()

      super.beforeEach()
    }
  }

  trait WithBuilder extends WithSuite with BeforeAndAfterEach {
    this: amber.mock.origin.BuilderComponent with Suite =>

    override def beforeEach() {
      super.beforeEach()

      when(build(anything(), anything(), anything())) thenAnswer {
        _: Array[AnyRef] => OriginFinder.add(built.last)
      }
    }
  }
}
