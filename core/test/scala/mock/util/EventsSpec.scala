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
package mock.util

import org.mockito.Mockito.verify

import amber.util.{EventsBehaviors, NotNothing}

class EventsSpec extends Spec with EventsBehaviors {

  override protected type Events[A] = amber.mock.util.Events[A]
  override val fixture = new Fixture {
    override def create[A : NotNothing : Manifest]() = Events[A]()
    override def emit[A](events: Events[A])(event: A) {events.emit(event)}
  }

  "mock.Events" should {
    behave like anEvents

    "allow verifying invocations of its subscribe method" in {
      val events = fixture.create[AnyRef]()
      val observe = amber.mock.util.Events.Observe[AnyRef]()

      val observer = events.subscribe(observe)

      try {
        verify(events).subscribe(observe)
      } finally {
        observer.dispose()
      }
    }

    "allow verifying invocations of its emit method" in {
      val event = new AnyRef
      val events = fixture.create[AnyRef]()

      events.emit(event)

      verify(events).emit(event)
    }
  }
}
