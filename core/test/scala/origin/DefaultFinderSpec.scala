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

class DefaultFinderSpec extends Spec
                        with mock.origin.BuilderComponent
                        with FinderComponent.Default
                        with FinderBehaviors {

  override val fixture = new Fixture {
    override def create(name: Property.Name) = builder.build(name, random[Family], () => None)
  }

  "Default.OriginFinder" should {
    behave like (aFinder forOrigins)

    "add an origin" when {
      "the origin is built" in {
        val origin = builder.build[Any, Origin.Read.Unfiltered[Any]](random[Property.Name], random[Family], () => None)

        origins.all() should contain(origin)
      }
    }
  }
}
