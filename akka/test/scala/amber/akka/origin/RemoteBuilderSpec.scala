/*
 * Copyright 2013 Sanjin Sehic
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
package akka
package origin

class RemoteBuilderSpec extends Spec("RemoteBuilderSpec")
                        with BuilderComponent.Remote
                        with amber.origin.BuilderBehaviors.Remote {

  override type Origin[+A] = Origin.Remote[A]

  override protected type Configuration = BuilderComponent.Remote.Configuration
  override protected object configuration extends Configuration {
    override val local = RemoteBuilderSpec.this.system
  }

  "akka.Remote.OriginBuilder" should {
    behave like aBuilder
  }
}
