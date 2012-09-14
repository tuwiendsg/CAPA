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

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.verify

class MemberFactorySpec extends Spec
                        with MemberFactoryComponent
                        with amber.family.MemberFactoryBehaviors {

  "mock.MemberFactory" should {
    behave like aFactory

    "allow verifying invocations of its create method" in {
      class A
      val name = random[Property.Name]
      val family = random[Family]
      val read = mock[Origin.Read.Filtered[A]]("Origin.read")

      in(family).create(name)(read)

      verify(in(family)).create(equalTo(name))(equalTo(read))(anything(), equalTo(manifest[A]))
    }
  }
}
