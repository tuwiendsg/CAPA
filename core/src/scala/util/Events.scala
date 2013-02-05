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

trait Events[+A] {
  def subscribe(f: Events.Observe[A]): Observer
}

object Events {

  type Observe[-A] = PartialFunction[A, Unit]

  def observe[A](events: Events[A])(f: Observe[A]) = events.subscribe(f)

  trait Enriched[A] {
    def ++[B >: A](that: Events[B]): Events[B]
    def map[B](f: A => B): Events[B]
    def filter(p: A => Boolean): Events[A]
  }

  implicit def toEnriched[A](source: Events[A]): Enriched[A] = new Enriched[A] {
    override def ++[B >: A](other: Events[B]) = {
      val sources = Array(source, other)
      new Events[B] {
        override def subscribe(f: Events.Observe[B]) = new Observer {
          val observers = sources map {_.subscribe(f)}
          override def dispose() {observers foreach {_.dispose()}}
        }
      }
    }

    override def map[B](f: A => B) = {
      new Events[B] {
        override def subscribe(g: Events.Observe[B]) = new Observer {
          val observer = source.subscribe {case a => g(f(a))}
          override def dispose() {observer.dispose()}
        }
      }
    }

    override def filter(p: A => Boolean) = {
      new Events[A] {
        override def subscribe(f: Events.Observe[A]) = new Observer {
          val observer = source.subscribe {case a if p(a) => f(a)}
          override def dispose() {observer.dispose()}
        }
      }
    }
  }
}
