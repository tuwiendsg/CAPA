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

class EventsSpec extends Spec {

  "Events.Enriched" when {
    "two sources are merged" should {
      trait Fixture {

        class A
        class B

        val eventsA = amber.mock.util.Events[A]()
        val eventsB = amber.mock.util.Events[B]()
        val observe = amber.mock.util.Events.Observe[AnyRef]()
      }

      "invoke a subscribed observer" when {
        "an event is emitted on the first source" in {
          new Fixture {
            val event = new A
            when(observe.isDefinedAt(event)) thenReturn true

            val observer = (eventsA ++ eventsB).subscribe(observe)
            try {
              eventsA.emit(event)
              verify(observe).apply(event)
            } finally {
              observer.dispose()
            }
          }
        }

        "an event is emitted on the second source" in {
          new Fixture {
            val event = new B
            when(observe.isDefinedAt(event)) thenReturn true

            val observer = (eventsA ++ eventsB).subscribe(observe)
            try {
              eventsB.emit(event)
              verify(observe).apply(event)
            } finally {
              observer.dispose()
            }
          }
        }
      }
    }

    "a source is mapped over with a mapper function" when {
      trait Fixture {

        class A
        class B

        val events = amber.mock.util.Events[A]()
        val observe = amber.mock.util.Events.Observe[AnyRef]()
        val mapper = mock[A => B]("Events.map")
      }

      "an event is emitted on the source" should {
        "invoke the mapper function" in {
          new Fixture {
            val event = new A
            when(observe.isDefinedAt(anything())) thenReturn false

            val observer = (events map mapper).subscribe(observe)
            try {
              events.emit(event)
              verify(mapper).apply(event)
            } finally {
              observer.dispose()
            }
          }
        }

        "invoke a subscribed observer with the result of the mapper function" in {
          new Fixture {
            val event = new B
            when(observe.isDefinedAt(event)) thenReturn true
            when(mapper.apply(anything())) thenReturn event

            val observer = (events map mapper).subscribe(observe)
            try {
              events.emit(new A)
              verify(observe).apply(event)
            } finally {
              observer.dispose()
            }
          }
        }
      }
    }

    "a source is filtered with a predicate function" when {
      trait Fixture {

        class A

        val event = new A
        val events = amber.mock.util.Events[A]()
        val observe = amber.mock.util.Events.Observe[AnyRef]()
        val predicate = mock[A => Boolean]("Events.filter")
      }

      "an event is emitted on the source" should {
        "invoke the predicate function" in {
          new Fixture {
            when(observe.isDefinedAt(anything())) thenReturn false

            val observer = (events filter predicate).subscribe(observe)
            try {
              events.emit(event)
              verify(predicate).apply(event)
            } finally {
              observer.dispose()
            }
          }
        }

        "invoke a subscribed observer" when {
          "the event passes the predicate function" in {
            new Fixture {
              when(observe.isDefinedAt(event)) thenReturn true
              when(predicate.apply(event)) thenReturn true

              val observer = (events filter predicate).subscribe(observe)
              try {
                events.emit(event)
                verify(observe).apply(event)
              } finally {
                observer.dispose()
              }
            }
          }
        }

        "not invoke a subscribed observer" when {
          "the event does not pass the predicate function" in {
            new Fixture {
              when(observe.isDefinedAt(event)) thenReturn true
              when(predicate.apply(event)) thenReturn false

              val observer = (events filter predicate).subscribe(observe)
              try {
                events.emit(event)
                verify(observe, never()).isDefinedAt(anything())
                verify(observe, never()).apply(anything())
              } finally {
                observer.dispose()
              }
            }
          }
        }
      }
    }
  }
}
