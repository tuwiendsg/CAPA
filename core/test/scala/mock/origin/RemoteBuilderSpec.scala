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

package at.ac.tuwien.infosys
package amber
package mock.origin

import util.Executors

class RemoteBuilderSpec extends Spec
                        with BuilderComponent.Remote.Default
                        with BuilderBehaviors.Remote {

  override protected type Configuration = amber.origin.BuilderComponent.Remote.Configuration
  override protected object configuration extends Configuration {
    override def context = Executors.CallingThread
  }

  "mock.Remote.Builder" should {
    behave like aMockBuilder
  }
}
