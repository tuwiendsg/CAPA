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
package akka

import _root_.akka.actor.Actor
import Actor.spawn

import amber.Origin.Meta
import amber.util.{Filter, Logger, NotNothing}
import Message.Request

private[akka] abstract class OriginActor[+A <: AnyRef : Manifest]
    (name: Property.Name, log: Logger) extends Actor {

  protected def read(filter: Filter[Meta.Readable]): Option[A]

  protected val meta = Meta.Writable()

  self.id = name.toString

  override protected def receive = {
    case Request.Value(filter) =>
      val replyTo = self.channel
      spawn {
        val value = read(filter)
        for (v <- value) log.debug("Read " + v + " for property " + name)
        replyTo tryTell (value map {Property(name, _)})
      }
    case get: Request.MetaInfo.Get[_] => self tryReply get(meta)
    case set: Request.MetaInfo.Set[_] => set(meta)
  }
}
