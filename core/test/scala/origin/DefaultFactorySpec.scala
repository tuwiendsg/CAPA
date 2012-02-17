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

import scalaz._
import Scalaz._

import org.mockito.Matchers.{any, anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{verify, when}

import util.NotNothing
import util.Events.observe

class DefaultFactorySpec extends Spec
                         with mock.origin.BuilderComponent
                         with FactoryComponent.Default
                         with FactoryBehaviors {

  "Default.OriginFactory" should {
    behave like aFactory

    "invoke the builder" in {
      val name = random[Property.Name]
      val read = () => none[AnyRef]

      origin.create(name)(read)

      verify(build).apply(equalTo(name), any(classOf[Family]), equalTo(read))
    }

    "return the result of the builder" in {
      val result = origin.create(random[Property.Name]) {() => none[AnyRef]}

      result should be(built.last)
    }

    "notify the creation of the origin" when {
      "an origin is built" in {
        class A
        val observed = amber.mock.util.Events.Observe[Any]()
        when(observed.isDefinedAt(anything())) thenReturn true
        val observer = observe(origin.created)(observed)

        try {
          val result = builder.build[A, Origin.Read.Unfiltered[A]](random[Property.Name], random[Family], () => None)
          verify(observed).apply((result, manifest[A]))
        } finally {
          observer.dispose()
        }
      }
    }
  }
}
