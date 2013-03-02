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

import scala.concurrent.{blocking, future}
import scala.reflect.runtime.universe.TypeTag

import _root_.akka.actor.Actor
import _root_.akka.pattern.pipe

import amber.util.{Filter, Logger}
import akka.Message.Request.{MetaInfo, Read}

private[akka] abstract class OriginActor[+A: TypeTag](name: Origin.Name, family: Origin.Family)
                                                     (log: Logger) extends Actor {

  protected def read(filter: Filter[Origin.Meta.Readable]): Option[A]

  import context.dispatcher

  protected val meta = new Origin.Meta.Writable.Default {
    override val name = OriginActor.this.name
    override val family = OriginActor.this.family
  }

  override def receive = {
    case Read(filter) =>
      future {
        for (value <- blocking {read(filter)}) yield {
          log.debug(s"Read $value from $name")
          Origin.Value(name, value)
        }
      } pipeTo sender
    case MetaInfo.Name => sender ! name
    case MetaInfo.Family => sender ! family
    case get: MetaInfo.Get[_] => sender ! get(meta)
    case set: MetaInfo.Set[_] => set(meta)
  }
}
