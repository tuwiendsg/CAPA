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

package amber
package family

import org.mockito.Mockito.when

import amber.util.Type

class DefaultFinderSpec extends Spec
                        with mock.origin.BuilderComponent.Local.Default
                        with mock.origin.BuilderComponent.InSpec
                        with FinderComponent.Default
                        with FinderBehaviors {

  override def mocker[A: Type] =
    super.mocker[A] andThen {case ((_, family, _), origin) =>
      when(origin.family) thenReturn family
    }

  override def build(family: Origin.Family) =
    builder.build(random[Origin.Name], family)(mock[OriginBuilder.Read[AnyRef]]("Origin.read"))

  "Default.FamilyFinder" should {
    behave like aFamiliesFinder
  }
}
