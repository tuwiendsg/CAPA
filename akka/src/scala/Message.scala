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

import scala.reflect.runtime.universe.TypeTag

import amber.util.Filter

private[akka] sealed trait Message

private[akka] object Message {
  object Request {

    case class Read(filter: Filter[Origin.Meta.Readable]) extends Message

    object MetaInfo {

      case object Name extends Message
      case object Family extends Message

      case class Get(name: Origin.MetaInfo.Name) extends Message {
        def apply(meta: Origin.Meta.Readable): Option[Origin.MetaInfo.Value[_]] =
          meta.selectDynamic(name)
      }

      case class Set[+A: TypeTag](name: Origin.MetaInfo.Name, value: A) extends Message {
        def apply(meta: Origin.Meta.Writable) {meta(name) = value}
      }
    }
  }
}
