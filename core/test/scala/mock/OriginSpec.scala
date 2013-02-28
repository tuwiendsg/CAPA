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
package mock

import org.mockito.Mockito.verify

import amber.util.Filter

class OriginSpec extends Spec with OriginBehaviors {

  val fixture = new OriginBehaviors.Fixture {

    override protected type Origin[+A <: AnyRef] = amber.Origin[A]

    override def create[A <: AnyRef : Manifest, B: amber.Origin.Read[A]#apply](name: Property.Name,
                                                                               family: Family,
                                                                               read: B) =
      Origin(name, family, read)
  }

  "mock.Origin" should {
    "create an origin" which {
      behave like (AnOrigin withName fixture)
      behave like (AnOrigin withFamily fixture)
      behave like (AnOrigin withMetaInfo fixture)
      behave like (AnOrigin withType fixture)
      behave like (AnOrigin withRead fixture)

      "allows verifying invocations of its apply method" in {
        val filter = mock[Filter[amber.Origin.Meta.Readable]]("Filter")
        val origin = fixture.create[AnyRef]()

        origin(filter)

        verify(origin).apply(filter)
      }
    }
  }
}
