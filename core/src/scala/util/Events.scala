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

import scala.language.implicitConversions

trait Events[+A] {
  def subscribe(f: Events.Observe[A]): Observer
}

object Events {

  type Observe[-A] = PartialFunction[A, Unit]

  def observe[A](events: Events[A])(f: Observe[A]) = events.subscribe(f)

  def merge[A, B <: A](events1: Events[A], events2: Events[B]): Events[A] = new Events[A] {
    override def subscribe(f: Events.Observe[A]) = new Observer {
      val observers = Array(events1.subscribe(f), events2.subscribe(f))
      override def dispose() {observers foreach {_.dispose()}}
    }
  }

  def map[A, B](events: Events[A])(f: A => B): Events[B] = new Events[B] {
    override def subscribe(g: Events.Observe[B]) = new Observer {
      val observer = events.subscribe {case a => g(f(a))}
      override def dispose() {observer.dispose()}
    }
  }

  def filter[A](events: Events[A])(p: A => Boolean): Events[A] = new Events[A] {
    override def subscribe(f: Events.Observe[A]) = new Observer {
      val observer = events.subscribe {case a if p(a) => f(a)}
      override def dispose() {observer.dispose()}
    }
  }

  implicit class Enriched[+A](val events: Events[A]) extends AnyVal {
    def ++[B >: A](other: Events[B]): Events[B] = merge(events, other)
    def map[B](f: A => B): Events[B] = Events.map(events)(f)
    def filter(p: A => Boolean): Events[A] = Events.filter(events)(p)
  }
}
