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

package amber
package origin

import scala.concurrent.Future

import scalaz.OptionT

import amber.util.Type

sealed trait BuilderBehaviors {
  this: Spec with OriginBehaviors with BuilderComponent =>

  def aBuilder() {
    "build an origin" which {
      behave like anOrigin
    }

    "build a mapped origin over an underlying origin" which {
      behave like aMappedOrigin(new Mapper {
        override def apply[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B) =
          builder.map(underlying, name)(f)
      })
    }
  }
}

object BuilderBehaviors extends {

  trait Local extends BuilderBehaviors with OriginBehaviors.Local {
    this: Spec with BuilderComponent.Local =>

    override def build[A: Type](name: Origin.Name, family: Origin.Family)(read: Fixture.Read[A]) =
      builder.build(name, family) {meta => read() map {a => (Origin.Value(name, a), meta)}}
  }

  trait Remote extends BuilderBehaviors with OriginBehaviors.Remote {
    this: Spec with BuilderComponent.Remote =>

    override def build[A: Type](name: Origin.Name, family: Origin.Family)(read: Fixture.Read[A]) =
      builder.build(name, family) {meta => OptionT(Future.successful(read() map {
        value => (Origin.Value(name, value), meta)
      }))}
  }
}
