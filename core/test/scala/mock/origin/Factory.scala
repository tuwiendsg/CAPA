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

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import org.scalatest.{BeforeAndAfterEach, Suite}
import org.scalatest.mock.MockitoSugar.mock

import util.Mocking
import util.NotNothing.notNothing

trait FactoryComponent extends amber.origin.FactoryComponent
                       with BeforeAndAfterEach
                       with Mocking {
  this: Suite =>

  override def beforeEach() {
    origin = mock[OriginFactory]("mock.OriginFactory")
    val created = amber.mock.util.Events[(Origin[_ <: AnyRef], Manifest[_ <: AnyRef])]

    when(origin.created) thenReturn created
    when(origin.create(anything())(anything())(anything(), anything())) thenAnswer {
      args: Array[AnyRef] =>
        val name = args(0).asInstanceOf[Property.Name]
        val read = args(1).asInstanceOf[Origin.Read.Unfiltered[AnyRef]]
        val manifest = args(3).asInstanceOf[Manifest[AnyRef]]

        val origin = amber.mock.Origin.create(name, read)(notNothing, manifest)
        created.emit((origin, manifest))

        origin
    }

    super.beforeEach()
  }

  override protected type Origin[+A <: AnyRef] = amber.Origin[A]
  var origin: OriginFactory = _
}
