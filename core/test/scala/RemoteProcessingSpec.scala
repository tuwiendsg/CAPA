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

import util.Executors

class RemoteProcessingSpec extends Spec
                           with mock.origin.BuilderComponent.Remote.Default
                           with origin.FinderComponent.Remote.Default
                           with family.FinderComponent.Default
                           with origin.FactoryComponent.Remote.Default
                           with Processing.Remote
                           with ProcessingBehaviors.Remote {

  override protected type Configuration = amber.origin.FinderComponent.Remote.Default.Configuration
  override protected object configuration extends Configuration {
    override val context = Executors.CallingThread
  }

  "Remote.Processing" should {
    behave like aProcessing
  }
}
