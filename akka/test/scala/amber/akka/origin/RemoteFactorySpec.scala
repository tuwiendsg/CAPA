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

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.{ActorRef, ActorSystem, Props}

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{verify, when}

import amber.util.{Events, Mocker, Type}

class RemoteFactorySpec extends Spec(ActorSystem("RemoteFactorySpec-Client",
                                     ConfigFactory.load.getConfig("client")))
                        with FactoryComponent.Remote
                        with amber.origin.DefaultFactoryBehaviors.Remote {

  val remote = ActorSystem("RemoteFactorySpec-Server", ConfigFactory.load.getConfig("server"))

  override def afterAll() {
    super.afterAll()
    remote.shutdown()
  }

  override protected type Configuration = FactoryComponent.Remote.Configuration
  override protected object configuration extends Configuration {
    override val local = RemoteFactorySpec.this.system
    override val remote = "akka://RemoteFactorySpec-Server@127.0.0.1:2552"
  }

  override protected val actor: FactoryComponent.Actor = _actor
  private object _actor extends FactoryComponent.Actor {
    override val factory = FactoryComponent.Actor.remote(system)(RemoteFactorySpec.this)
  }

  object local extends amber.mock.origin.BuilderComponent.Local
               with akka.origin.FactoryComponent.Local {

      override protected type Configuration = FactoryComponent.Local.Configuration
      override protected object configuration extends Configuration {
        override val system = RemoteFactorySpec.this.remote
      }

      override protected type Origin[+A] = Origin.Local[A]

      override protected val actor: FactoryComponent.Actor = _actor
      private object _actor extends FactoryComponent.Actor {
        override val factory = FactoryComponent.Actor.local(remote)(local.this)
      }

      override def mocker[A](implicit typeA: Type[A]) =
        new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]] {
          def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A])) = {
            val origin = RemoteFactorySpec.this.mock[Origin[A]](s"mock.Origin.Local[$typeA]")
            val name = args._1
            val family = args._2

            when(origin.name) thenReturn name
            when(origin.family) thenReturn family
            when(origin.returns(anything(), anything())) thenAnswer {
              args: Array[AnyRef] => typeA <:< args(1).asInstanceOf[Type[_]]
            }
            when(origin.writeReplace()) thenAnswer {
              _: Array[AnyRef] =>
                new Origin.Serialized[A](name, family)(
                  RemoteFactorySpec.this.mock[ActorRef]("mock.ActorRef")
                )
            }

            origin
          }
        }
    }
  local // force initialization

  "akka.Remote.OriginFactory" should {
    behave like aFactory

    "notify the creation of the origin" when {
      "an origin is created remotely" in {
        new Fixture {
          val observe = mock[Events.Observe[Any]]("Events.observe")
          when(observe.isDefinedAt(anything())) thenReturn true
          val observer = Events.observe(origin.created)(observe)

          try {
            val read = mock[local.OriginFactory.Read[AnyRef]]("Origin.read")
            val result = local.origin.create(name)(read)
                           .writeReplace().asInstanceOf[Origin.Serialized[AnyRef]]
                           .toRemote(RemoteFactorySpec.this)(configuration.context, timeout)
            Thread.sleep(timeout.toMillis) // give sime time for netty to receive the message
            verify(observe).apply(result)
          } finally {
            observer.dispose()
          }
        }
      }
    }
  }
}
