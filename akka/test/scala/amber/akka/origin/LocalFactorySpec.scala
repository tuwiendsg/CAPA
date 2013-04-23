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
package akka
package origin

import org.mockito.Mockito.when

import amber.util.{Mocker, Type}

class LocalFactorySpec extends Spec("LocalFactorySpec")
                       with amber.mock.origin.BuilderComponent.Local
                       with FactoryComponent.Local
                       with amber.origin.DefaultFactoryBehaviors.OnMockBuilder.Local {

  override protected type Configuration = FactoryComponent.Local.Configuration
  override protected object configuration extends Configuration {
    override val system = LocalFactorySpec.this.system
  }

  override protected type Origin[+A] = Origin.Local[A]

  override protected val actor: FactoryComponent.Actor = _actor
  private object _actor extends FactoryComponent.Actor {
    override val factory = FactoryComponent.Actor.local(system)(LocalFactorySpec.this)
  }

  override def mocker[A](implicit typeA: Type[A]) =
    new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]] {
      def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A])) = {
        val origin = LocalFactorySpec.this.mock[Origin[A]](s"mock.Origin.Local[$typeA]")
        when(origin.name) thenReturn args._1
        origin
      }
    }

  "akka.Local.OriginFactory" should {
    behave like aFactory
  }
}
