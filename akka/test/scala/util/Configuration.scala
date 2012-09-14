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

import _root_.akka.actor.Actor
import _root_.akka.dispatch.MessageDispatcher
import _root_.akka.testkit.CallingThreadDispatcher

object Configuration {

  object Defaults {

    val MessageDispatcher = CallingThreadDispatcher.global

    def actorOf(factory: => Actor) = {
      val actor = Actor.actorOf(factory)

      actor.dispatcher = MessageDispatcher
      actor.start()

      actor
    }
  }

  class MessageDispatcherConfigurator extends _root_.akka.dispatch.MessageDispatcherConfigurator {
    def configure(config: _root_.akka.config.Configuration) = Defaults.MessageDispatcher
  }
}
