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

import org.mockito.Mockito.{verify, when}

class DelegatorFinderSpec extends Spec
                          with FinderComponent.Delegator
                          with FinderBehaviors {

  override val finder = new FinderComponent
  class FinderComponent extends amber.origin.FinderComponent {
    override type Origin[+A] = amber.Origin[A]
    override val origins = mock[OriginFinder]("origin.Finder")
  }

  override val fixture = new Fixture {
    def create(name: Property.Name) = {
      val origin = mock[Origin[_]]("Origin")
      when(origin.name) thenReturn name
      origin
    }
  }

  "OriginFinder.Delegator" should {
    "invoke the delegatee's find method" in {
      val name = random[Property.Name]

      origins.find(name)

      verify(finder.origins).find(name)
    }

    "invoke the delegatee's all method" in {
      origins.all()

      verify(finder.origins).all()
    }
  }
}
