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

private[akka] sealed trait Message extends Serializable

private[akka] object Message {
  object Request {

    case object Read extends Message

    object MetaInfo {
      case object Name extends Message
      case object Family extends Message
      case class Get(name: Origin.MetaInfo.Name) extends Message
      case class Set[+A](name: Origin.MetaInfo.Name, value: Origin.MetaInfo.Value[A])
        extends Message
    }
  }
}
