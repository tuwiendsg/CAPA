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

import org.scalatest.BeforeAndAfterEach
import org.scalatest.mock.MockitoSugar.mock

import util.Mocker

trait BuilderComponent extends amber.origin.BuilderComponent
                       with BeforeAndAfterEach {
  this: Spec =>

  override def beforeEach() {
    built = List.empty
    build = mock[(Property.Name, Family, Any) => Unit]("mock.OriginBuilder.build")

    super.beforeEach()
  }

  var built: List[Origin[_]] = _
  var build: (Property.Name, Family, Any) => Unit = _

  def mocker[A: Manifest, B: Origin.Read[A]#apply] =
    new Mocker[(Property.Name, Family, B, Manifest[A]), amber.Origin[A]] {
      def mock(args: (Property.Name, Family, B, Manifest[A])) =
        org.scalatest.mock.MockitoSugar.mock[amber.Origin[A]]("mock.Origin[" + manifest[A] + "]")
    }

  override protected type Origin[+A] = amber.Origin[A]
  override protected def builder = new OriginBuilder {
    override def build[A: Manifest, B: Origin.Read[A]#apply](name: Property.Name,
                                                             family: Family,
                                                             read: B) = {
      val origin = mocker[A, B].mock(name, family, read, manifest[A])

      built = built :+ origin
      BuilderComponent.this.build(name, family, read)

      origin
    }
  }
}
