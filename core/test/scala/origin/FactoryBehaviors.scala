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

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{verify, when}

import util.NotNothing
import util.Events.observe

trait FactoryBehaviors extends OriginBehaviors {
  this: Spec with FactoryComponent =>

  val fixture = new OriginBehaviors.Fixture.WithFilteredRead {

    override protected type Origin[+A <: AnyRef] = FactoryBehaviors.this.Origin[A]

    override def create[A <: AnyRef : NotNothing : Manifest](name: Property.Name, family: Family) =
      origin.create[A](name) {() => None}

    override def create[A <: AnyRef : Manifest](read: Origin.Read.Unfiltered[A]) =
      origin.create[A](random[Property.Name])(read)
  }

  def anOrigin() {
    behave like (AnOrigin withName fixture)
    behave like (AnOrigin withMetaInfo fixture)
    behave like (AnOrigin withType fixture)
    behave like (AnOrigin withFilteredRead fixture)
  }

  def aFactory() {
    "return the origin" which {
      behave like anOrigin
    }

    "notify the creation of the origin" when {
      "an origin is created" in {
        class A
        val observed = amber.mock.util.Events.Observe[Any]()
        when(observed.isDefinedAt(anything())) thenReturn true
        val observer = observe(origin.created)(observed)

        try {
          val result = origin.create[A](random[Property.Name]) {() => None}
          verify(observed).apply((result, manifest[A]))
        } finally {
          observer.dispose()
        }
      }
    }
  }
}
