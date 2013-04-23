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

import scala.language.higherKinds

import scala.concurrent.Future

import scalaz.Id.Id

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import util.{NotNothing, Type}

sealed trait DefaultFinderBehaviors[X[+_]] extends FinderBehaviors[X] {
  this: Spec with origin.BuilderComponent with FinderComponent.Default[X] =>

  override def build[A: NotNothing : Type](name: Origin.Name) =
    builder.build(name, random[Origin.Family])(mock[OriginBuilder.Read[A]]("Origin.read"))
}

object DefaultFinderBehaviors {

  trait Local extends DefaultFinderBehaviors[Id] with FinderBehaviors.Local {
    this: Spec with origin.BuilderComponent.Local
               with FinderComponent.Local
               with FinderComponent.Default[Id] =>
  }

  trait Remote extends DefaultFinderBehaviors[Future] with FinderBehaviors.Remote {
    this: Spec with origin.BuilderComponent.Remote
               with FinderComponent.Remote
               with FinderComponent.Default[Future] =>
  }

  sealed trait OnMockBuilder[X[+_]] extends mock.origin.BuilderComponent
                             with mock.origin.BuilderComponent.InSpec {
    this: Spec with FinderComponent.Default[X] with DefaultFinderBehaviors[X] =>

    abstract override def mocker[A](implicit typeA: Type[A]) =
      super.mocker[A] andThen {case ((name, family, _), origin) =>
        when(origin.name) thenReturn name
        when(origin.family) thenReturn family
        when(origin.returns(anything(), anything())) thenAnswer {
          args: Array[AnyRef] => typeA <:< args(1).asInstanceOf[Type[_]]
        }
      }
  }

  object OnMockBuilder {

    trait Local extends OnMockBuilder[Id] with mock.origin.BuilderComponent.Local {
      this: Spec with FinderComponent.Local.Default with DefaultFinderBehaviors.Local =>
    }

    trait Remote extends OnMockBuilder[Future] with mock.origin.BuilderComponent.Remote {
      this: Spec with FinderComponent.Remote.Default with DefaultFinderBehaviors.Remote =>
    }
  }
}
