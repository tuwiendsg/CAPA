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

import java.util.concurrent.CopyOnWriteArrayList

import scala.collection.JavaConversions._

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{doAnswer, when}

import org.scalatest.mock.MockitoSugar.mock

import util.{Mocking, NotNothing, Observer}
import util.Events.Observe

trait Events[A] extends amber.util.Events[A] {
  def emit(event: A)
}

object Events extends Mocking {

  def apply[A: NotNothing : Manifest](): Events[A] = {
    val observers = new CopyOnWriteArrayList[Observe[A]]
    val events = mock[Events[A]]("mock.Events")

    when(events.subscribe(anything())) thenAnswer {
      args: Array[AnyRef] =>
        val f = args(0).asInstanceOf[Observe[A]]
        observers.add(f)
        new Observer {override def dispose() {observers.remove(f)}}
    }
    doAnswer({
      args: Array[AnyRef] =>
        val event = args(0).asInstanceOf[A]
        for (observer <- observers if observer.isDefinedAt(event))
          observer(event)
    }).when(events).emit(anything())

    events
  }

  object Observe {
    def apply[A: Manifest]() = mock[Observe[A]]("mock.Events.Observe")
  }
}
