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

import scala.reflect.ClassTag

import org.mockito.Mockito.verify

class BuilderSpec extends Spec
                  with BuilderComponent {

  trait Fixture {

    val name = random[Origin.Name]
    val family = random[Origin.Family]
    val read = mock[Origin.Read.Filtered[AnyRef]]("Origin.read")

    object origin {
      def build(): Origin[_] = builder.build(name, family, read)
    }
  }

  "mock.OriginBuilder" when {
    "an origin is built" should {
      "invoke the mocked build method" in {
        new Fixture{
          origin.build()

          verify(build).apply(name, family, read)
        }
      }

      "save the origin that was last built" in {
        new Fixture {
          val result = origin.build()

          built.last should be(result)
        }
      }
    }
  }
}
