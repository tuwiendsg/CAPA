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

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{verify, when}

import util.Events

trait FactoryBehaviors {
  this: Spec with FactoryComponent =>

  trait Fixture {

    class A

    val name = random[Origin.Name]
    val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
  }

  def aFactory() {
    "notify the creation of the origin" when {
      "an origin is created" in {
        new Fixture {
          val observe = mock[Events.Observe[Any]]("Events.observe")
          when(observe.isDefinedAt(anything())) thenReturn true
          val observer = Events.observe(origin.created)(observe)

          try {
            val result = origin.create(name)(read)
            verify(observe).apply(result)
          } finally {
            observer.dispose()
          }
        }
      }
    }
  }
}
