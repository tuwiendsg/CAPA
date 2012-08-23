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

class DefaultMemberFactorySpec extends Spec
                               with mock.origin.BuilderComponent
                               with mock.family.FinderComponent.WithBuilder
                               with MemberFactoryComponent.Default
                               with MemberFactoryBehaviors {

  "Default.MemberFactory" should {
    behave like aFactory

    "invoke the builder" in {
      val name = random[Property.Name]
      val family = random[Family]
      val read: Origin.Read.Filtered[AnyRef] = _ => None

      in(family).create(name)(read)

      verify(build).apply(name, family, read)
    }

    "return the result of the build method" in {
      val result = in(random[Family]).create[AnyRef](random[Property.Name])(_ => None).value

      result should be(built.last)
    }

    "not invoke the builder for the second time" when {
      "there is already a same member in the origin family" in {
        class A
        val name = random[Property.Name]
        val family = random[Family]
        in(family).create[A](name)(_ => None) should be('defined)

        in(family).create[A](name) {_ => None}

        verify(build, times(1)).apply(equalTo(name), equalTo(family), anything())
      }
    }
  }
}
