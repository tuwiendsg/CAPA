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
package mock.origin

import org.mockito.Mockito.{verify, when}

import amber.util.Type

sealed trait BuilderBehaviors extends BuilderComponent.InSpec {
  this: Spec with BuilderComponent =>

  trait Fixture {

    val name = random[Origin.Name]
    val family = random[Origin.Family]

    object origin {

      def build(): Origin[_] = {
        val origin = builder.build(name, family)(mock[OriginBuilder.Read[AnyRef]]("Origin.read"))
        when(origin.family) thenReturn family
        origin
      }

      def map[A, B: Type](underlying: Origin[A], name: Origin.Name): Origin[_] = {
        builder.map(underlying, name)(mock[A => B]("Origin.map"))
      }
    }
  }

  def aMockBuilder() {
    "invoke the mocked build method" when {
      "an origin is built" in {
        new Fixture{
          origin.build()

          verify(build).apply(name, family)
        }
      }
    }

    "save the origin that was last built" in {
      new Fixture {
        val result = origin.build()

        built.last should be(result)
      }
    }

    "an origin is mapped over an underlying origin" should {
      "invoke the mocked build method" in {
        new Fixture{
          val other = random[Origin.Name]
          origin.map(origin.build(), other)

          verify(build).apply(other, family)
        }
      }

      "save the origin that was last built" in {
        new Fixture {
          val result = origin.map(origin.build(), name)

          built.last should be(result)
        }
      }
    }
  }
}

object BuilderBehaviors {

  trait Local extends BuilderBehaviors {
    this: Spec with BuilderComponent.Local =>
  }

  trait Remote extends BuilderBehaviors {
    this: Spec with BuilderComponent.Remote =>
  }
}
