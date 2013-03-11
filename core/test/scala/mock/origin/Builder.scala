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
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, typeTag, TypeTag}

import org.scalatest.BeforeAndAfterEach

import util.Mocker

trait BuilderComponent extends amber.origin.BuilderComponent
                       with BeforeAndAfterEach {
  this: Spec =>

  override def beforeEach() {
    built = List.empty
    build = mock[(Origin.Name, Origin.Family) => Unit]("mock.OriginBuilder.build")

    super.beforeEach()
  }

  var built: List[Origin[_]] = _
  var build: (Origin.Name, Origin.Family) => Unit = _

  def mocker[A: ClassTag : TypeTag] =
    new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A], TypeTag[A]), Origin[A]] {
      def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A], TypeTag[A])) =
        BuilderComponent.this.mock[Origin.Local[A]](s"mock.Origin.Local[${typeOf[A]}]")
    }

  override protected type Origin[+A] = amber.Origin.Local[A]
  override protected def builder = new OriginBuilder {
    override def build[A: ClassTag : TypeTag](name: Origin.Name, family: Origin.Family)
                                             (read: OriginBuilder.Read[A]) = {
      val origin = mocker[A].mock(name, family, read, typeTag[A])
      built = built :+ origin
      BuilderComponent.this.build(name, family)

      origin
    }
  }
}
