/*
 * Copyright 2013 Sanjin Sehic
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

import scala.collection.JavaConversions._

import _root_.akka.actor.Props

import amber.util.Type

class RemoteOriginSpec extends Spec("RemoteOriginSpec")
                       with OriginBehaviors.Remote {

  override type Origin[+A] = Origin.Remote[A]

  override val fixture = new Fixture {
    override def create[A](name: Origin.Name, family: Origin.Family, _read: Fixture.Read[A])
                          (implicit typeA: Type[A]) = {
      Origin.Remote[A](name, family)(system.actorOf(
          Props(new Origin.Actor.Local(
            new Origin.Local.Default(name, family) {
              override def read() = for {value <- _read()} yield (Origin.Value(name, value), meta)
            }
          )).withDispatcher("amber.origins.dispatcher")
        ))(typeA, timeout)
    }
  }

  "akka.Origin.Remote is an remote origin" which {
    behave like anOrigin
  }
}
