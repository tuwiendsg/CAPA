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
package util

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{never, verify, when}

trait EventsBehaviors {
  this: Spec =>

  type Events[A] <: amber.util.Events[A]
  def fixture: Fixture

  trait Fixture {
    def create[A: NotNothing : Manifest](): Events[A]
    def emit[A](events: Events[A])(event: A)
  }

  def anEvents() {
    class A

    "invoke a subscribed observer" in {
      val event = new A
      val events = fixture.create[A]()
      val observe = mock[Events.Observe[A]]("Events.observe")
      when(observe.isDefinedAt(event)) thenReturn true

      val observer = events.subscribe(observe)
      try {
        fixture.emit(events)(event)
        verify(observe).apply(event)
      } finally {
        observer.dispose()
      }
    }

    "not invoke a subscribed observer" when {
      "it is not defined for the event" in {
        val event = new A
        val events = fixture.create[A]()
        val observe = mock[Events.Observe[A]]("Events.observe")
        when(observe.isDefinedAt(event)) thenReturn false

        val observer = events.subscribe(observe)
        try {
          fixture.emit(events)(event)
          verify(observe, never()).apply(anything())
        } finally {
          observer.dispose()
        }
      }

      "it is disposed" in {
        val event = new A
        val events = fixture.create[A]()
        val observe = mock[Events.Observe[A]]("Events.observe")

        val observer = events.subscribe(observe)
        observer.dispose()
        fixture.emit(events)(event)

        verify(observe, never()).isDefinedAt(anything())
        verify(observe, never()).apply(anything())
      }
    }
  }
}
