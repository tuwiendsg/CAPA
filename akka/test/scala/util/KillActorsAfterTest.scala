/*
 * Copyright 2011 Sanjin Sehic
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
package util

import _root_.akka.dispatch.MessageDispatcher

import org.scalatest.{BeforeAndAfterEach, Suite}

trait KillActorsAfterTest extends BeforeAndAfterEach {
  this: Suite =>

  abstract override protected def afterEach() {
    super.afterEach()

    Configuration.Defaults.MessageDispatcher.stopAllAttachedActors
    MessageDispatcher.defaultGlobalDispatcher.stopAllAttachedActors
  }
}
