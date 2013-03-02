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
package akka
package origin

import scala.reflect.runtime.universe.TypeTag

import amber.util.NoLogging

class BuilderSpec extends Spec("BuilderSpec")
                  with BuilderComponent
                  with NoLogging
                  with OriginBehaviors {

  override type Configuration = BuilderComponent.Configuration
  override object configuration extends BuilderComponent.Configuration {
    override val system = BuilderSpec.this.system
  }

  override val fixture = new Fixture {

    protected type Origin[+A] = BuilderSpec.this.Origin[A]

    override def create[A: Manifest : TypeTag, B: Origin.Read[A]#apply](name: Origin.Name,
                                                                        family: Origin.Family,
                                                                        read: B) =
      builder.build(name, family, read)
  }

  "akka.OriginBuilder" when {
    "an origin is built" should {
      "return the origin" which {
        behave like anOrigin
      }
    }
  }
}
