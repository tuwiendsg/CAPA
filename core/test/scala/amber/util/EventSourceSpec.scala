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

package amber
package util

class EventSourceSpec extends Spec with EventsBehaviors {

  override type Events[A] = EventSource[A]

  override def create() = EventSource[A]()
  override def emit(events: Events[A])(event: A) {events.emit(event)}

  "EventSource" should {
    behave like anEvents
  }
}
