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

import util.NotNothing

trait BuilderBehaviors extends OriginBehaviors {
  this: Spec with BuilderComponent =>

  val fixture = new OriginBehaviors.Fixture {

    protected type Origin[+A <: AnyRef] = BuilderBehaviors.this.Origin[A]

    override def create[A <: AnyRef : NotNothing : Manifest, B: Origin.Read[A]#apply]
        (name: Property.Name, family: Family, read: B) =
      builder.build(name, family, read)
  }

  def anOrigin() {
    behave like (AnOrigin withName fixture)
    behave like (AnOrigin withFamily fixture)
    behave like (AnOrigin withMetaInfo fixture)
    behave like (AnOrigin withType fixture)
    behave like (AnOrigin withRead fixture)
  }

  object aBuilder {
    def onBuild() {
      "return the origin" which {
        behave like anOrigin
      }
    }
  }
}
