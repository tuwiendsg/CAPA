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

import java.util.concurrent.CopyOnWriteArrayList

import scala.collection.JavaConversions._

class EventSource[A: NotNothing] extends Events[A] {

  private val observers = new CopyOnWriteArrayList[Observer]

  override def subscribe(f: Events.Observe[A]) = {
    val observer = new Observer(f)
    add(observer)
    observer
  }

  def emit(event: A) {
    for (observer <- observers) {observer(event)}
  }

  protected def add(observer: Observer) {
    observers.add(observer)
  }

  protected def remove(observer: Observer) {
    observers.remove(observer)
  }

  class Observer(f: Events.Observe[A]) extends util.Observer {

    private[EventSource] def apply(event: A) {
      if (f.isDefinedAt(event)) f(event)
    }

    override def dispose() {EventSource.this.remove(this)}
  }
}

object EventSource {
  def apply[A: NotNothing]() = new EventSource[A]
}
