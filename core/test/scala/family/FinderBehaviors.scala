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
package family

trait FinderBehaviors {
  this: Spec with FinderComponent =>

  def fixture: Fixture

  trait Fixture {
    def create(family: Family): Origin[_]
  }

  object aFinder {
    def forFamilies() {
      "find an origin" when {
        "using the origin's family" in {
          val family = random[Family]
          val origin = fixture.create(family)

          families.find(family) should contain(origin)
        }
      }

      "not find an origin" when {
        "using a family different from the origin's family" in {
          val family = random[Family]
          val origin = fixture.create(family)

          families.find(different(family)) should not(contain(origin))
        }
      }

      "return all origins" in {
        val origin = fixture.create(random[Family])

        families.all() should contain(origin)
      }
    }
  }
}
