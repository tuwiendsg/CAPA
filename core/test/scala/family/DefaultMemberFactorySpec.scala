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

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{times, verify, when}

import util.Type

class DefaultMemberFactorySpec extends Spec
                               with mock.origin.BuilderComponent
                               with family.FinderComponent.Default
                               with MemberFactoryComponent.Default
                               with MemberFactoryBehaviors {

  override def mocker[A](implicit typeA: Type[A]) =
    super.mocker[A] andThen {case ((name, family, _), origin) =>
      when(origin.name) thenReturn name
      when(origin.family) thenReturn family
      when(origin.returns(anything(), equalTo(typeA))) thenReturn true
    }

  "Default.MemberFactory" should {
    behave like aFactory

    "invoke the builder" in {
      new Fixture {
        in(family).create(name)(read)

        verify(build).apply(name, family)
      }
    }

    "return the result of the build method" in {
      new Fixture {
        in(family).create(name)(read).value should be(built.last)
      }
    }

    "not invoke the builder for the second time" when {
      "there is already a same member in the origin family" in {
        new Fixture {
          in(family).create(name)(read)
          in(family).create(name)(read)

          verify(build, times(1)).apply(name, family)
        }
      }
    }
  }
}
