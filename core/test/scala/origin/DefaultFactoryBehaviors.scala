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
package origin

import org.mockito.Matchers.{any, anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{verify, when}

import util.Events

sealed trait DefaultFactoryBehaviors extends FactoryBehaviors {
  this: Spec with origin.BuilderComponent with FactoryComponent.Default =>

  override def aFactory() {
    super.aFactory()

    "notify the creation of the origin" when {
      "an origin is built" in {
        new Fixture {
          val observe = mock[Events.Observe[Any]]("Events.observe")
          when(observe.isDefinedAt(anything())) thenReturn true
          val observer = Events.observe(origin.created)(observe)

          try {
            val read = mock[OriginBuilder.Read[AnyRef]]("Origin.read")
            val result = builder.build(name, random[Origin.Family])(read)
            verify(observe).apply(result)
          } finally {
            observer.dispose()
          }
        }
      }

      "an origin is mapped" in {
        new Fixture {
          val observe = mock[Events.Observe[Any]]("Events.observe")
          when(observe.isDefinedAt(anything())) thenReturn true
          val observer = Events.observe(origin.created)(observe)

          try {
            val read = mock[OriginBuilder.Read[AnyRef]]("Origin.read")
            val underlying = builder.build(name, random[Origin.Family])(read)
            val result = builder.map(underlying, name) {_ => random[Int]}
            verify(observe).apply(result)
          } finally {
            observer.dispose()
          }
        }
      }
    }
  }
}

object DefaultFactoryBehaviors {

  trait Local extends DefaultFactoryBehaviors  {
    this: Spec with origin.BuilderComponent.Local
               with FactoryComponent.Local
               with FactoryComponent.Default =>
  }

  trait Remote extends DefaultFactoryBehaviors {
    this: Spec with origin.BuilderComponent.Remote
               with FactoryComponent.Remote
               with FactoryComponent.Default =>
  }

  sealed trait OnMockBuilder extends mock.origin.BuilderComponent.InSpec
                             with DefaultFactoryBehaviors {
    this: Spec with mock.origin.BuilderComponent with FactoryComponent.Default =>

    override def aFactory() {
      super.aFactory()

      "invoke the builder" in {
        new Fixture {
          origin.create(name)(read)

          verify(build).apply(equalTo(name), any(classOf[Origin.Family]))
        }
      }

      "return the result of the builder" in {
        new Fixture {
          origin.create(name)(read) should be(built.last)
        }
      }
    }
  }

  object OnMockBuilder {

    trait Local extends OnMockBuilder with DefaultFactoryBehaviors.Local {
      this: Spec with mock.origin.BuilderComponent.Local with FactoryComponent.Local.Default =>
    }

    trait Remote extends OnMockBuilder with DefaultFactoryBehaviors.Remote {
      this: Spec with mock.origin.BuilderComponent.Remote with FactoryComponent.Remote.Default =>
    }
  }
}
