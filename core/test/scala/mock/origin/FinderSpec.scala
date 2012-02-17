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

import org.mockito.Mockito.verify

class FinderSpec extends Spec
                 with FinderComponent.WithSuite
                 with amber.origin.FinderBehaviors {

  override protected type Origin[+A <: AnyRef] = amber.Origin[A]
  override val fixture = new Fixture {
    override def create(name: Property.Name) = {
      val origin = amber.mock.Origin(name = name)
      OriginFinder.add(origin)
      origin
    }
  }

  "mock.OriginFinder" should {
    behave like (aFinder forOrigins)

    "allow verifying invocations of its find method" in {
      val name = random[Property.Name]

      origins.find(name)

      verify(origins).find(name)
    }

    "allow verifying invocations of its all method" in {
      origins.all()

      verify(origins).all()
    }
  }
}
