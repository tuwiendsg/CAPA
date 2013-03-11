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

import util.Events

trait System extends origin.FinderComponent.Local
             with origin.FactoryComponent
             with family.MemberFactoryComponent
             with Processing
             with Processing.Default.Conversions
             with Processing.Default.Operations {

  def stopped: Events[Unit]

  def client: Client = _client
  def shutdown() {
    process.shutdown()
  }

  trait Client extends Client.Local with amber.origin.FinderComponent.Delegator.Local {
    override protected val finder = System.this
  }

  private object _client extends Client
}
