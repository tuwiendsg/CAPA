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

trait FinderBehaviors {
  this: Spec with FinderComponent =>

  def fixture: Fixture

  trait Fixture {
    def create(name: Origin.Name): Origin[_]
  }

  object aFinder {
    def forOrigins() {
      "find an origin" when {
        "using the origin's name" in {
          val name = random[Origin.Name]
          val origin = fixture.create(name)

          origins.find(name) should contain(origin)
        }

        "using the name of an origin's parent" in {
          val parent = random[Origin.Name]
          val origin = fixture.create(parent / random[String])

          origins.find(parent) should contain(origin)
        }
      }

      "not find an origin" when {
        "using a name different from the origin's name" in {
          val name = random[Origin.Name]
          val origin = fixture.create(name)

          origins.find(different(name)) should not(contain(origin))
        }

        "using the name of an origin's child" in {
          val name = random[Origin.Name]
          val origin = fixture.create(name)

          origins.find(name / random[String]) should not(contain(origin))
        }
      }

      "return all origins" in {
        val origin = fixture.create(random[Origin.Name])

        origins.all() should contain(origin)
      }
    }
  }
}
