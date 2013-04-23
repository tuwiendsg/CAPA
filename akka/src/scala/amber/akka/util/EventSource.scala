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
package akka
package util

import scala.reflect.ClassTag

import _root_.akka.actor.{ActorRef, ActorRefFactory, PoisonPill, Props}

import amber.util.{Events, NotNothing}

private[akka] class EventSource[A: NotNothing : ClassTag](factory: ActorRefFactory)
    extends amber.util.EventSource[A] {

  override def subscribe(f: Events.Observe[A]) = subscribe(factory.actorOf(
      Props(new Observer.Notifier[A](f)).withDispatcher("amber.observers.dispatcher")
  ))

  private[akka] def subscribe(reference: ActorRef): Observer = {
    val observer = new Observer(reference)
    add(observer)
    observer
  }

  class Observer(ref: ActorRef) extends super.Observer(forwardTo(ref)) {
    override def dispose() {
      super.dispose()
      ref ! PoisonPill
    }
  }

  private def forwardTo(ref: ActorRef): Events.Observe[A] = {
    case any => ref ! any
  }
}

private[akka] object EventSource {
  def apply[A: NotNothing : ClassTag](factory: ActorRefFactory) = new EventSource[A](factory)
}
