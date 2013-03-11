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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.mockito.Mockito.when

class DefaultFinderSpec extends Spec
                        with mock.origin.BuilderComponent
                        with FinderComponent.Local.Default
                        with FinderBehaviors.Local {

  override def mocker[A: ClassTag : TypeTag] =
    super.mocker[A] andThen {case ((name, family, _, _), origin) =>
      when(origin.name) thenReturn name
      when(origin.family) thenReturn family
    }

  override val fixture = new Fixture {
    override def create(name: Origin.Name) = {
      val family = random[Origin.Family]
      val read = mock[OriginBuilder.Read[AnyRef]]("Origin.read")

      builder.build(name, family)(read)
    }
  }

  "Default.OriginFinder" should {
    behave like aFinder.forOrigins
  }
}
