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

import scala.collection.immutable.List

import org.scalatest.{BeforeAndAfterEach, Suite}
import org.scalatest.mock.MockitoSugar.mock

import util.NotNothing

trait BuilderComponent extends amber.origin.BuilderComponent
                       with BeforeAndAfterEach {
  this: Suite =>

  override def beforeEach() {
    built = List.empty
    build = mock[(Property.Name, Family, Any) => Unit]("mock.OriginBuilder.build")

    super.beforeEach()
  }

  var built: List[Origin[AnyRef]] = _
  var build: (Property.Name, Family, Any) => Unit = _

  override protected type Origin[+A <: AnyRef] = amber.Origin[A]
  override protected def builder = new OriginBuilder {
    override def build[A <: AnyRef : NotNothing : Manifest, B: Origin.Read[A]#apply]
        (name: Property.Name, family: Family, read: B): Origin[A] = {
      val origin = amber.mock.Origin(name, family, read)

      built = built :+ origin
      BuilderComponent.this.build(name, family, read)

      origin
    }
  }
}
