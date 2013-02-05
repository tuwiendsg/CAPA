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
package family

import util.NotNothing

trait MemberFactoryBehaviors extends OriginBehaviors {
  this: Spec with MemberFactoryComponent =>

  val fixture = new OriginBehaviors.Fixture.WithUnfilteredRead {

    override protected type Origin[+A <: AnyRef] = MemberFactoryBehaviors.this.Origin[A]

    override def create[A <: AnyRef : NotNothing : Manifest](name: Property.Name, family: Family) =
      (in(family).create[A](name) {_ => None}).value

    override def create[A <: AnyRef : NotNothing : Manifest](read: Origin.Read.Filtered[A]) =
      in(random[Family]).create[A](random[Property.Name])(read).value
  }

  def anOrigin() {
    behave like (AnOrigin withName fixture)
    behave like (AnOrigin withFamily fixture)
    behave like (AnOrigin withMetaInfo fixture)
    behave like (AnOrigin withType fixture)
    behave like (AnOrigin withUnfilteredRead fixture)
  }

  def aFactory() {
    "return an origin" which {
      behave like anOrigin
    }

    "return None" when {
      "there is already a same member in the origin family" in {
        class A
        val name = random[Property.Name]
        val family = random[Family]
        in(family).create[A](name)(_ => None) should be('defined)

        in(family).create[A](name)(_ => None) should not be('defined)
      }
    }
  }
}
