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

import _root_.akka.actor.{Actor, ActorRef, ActorSystem, Props}

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import amber.util.{Mocker, NotNothing, Type}

class RemoteFinderSpec extends Spec(ActorSystem("RemoteFinderSpec-Client",
                                    ConfigFactory.load.getConfig("client")))
                       with FinderComponent.Remote
                       with amber.origin.DefaultFinderBehaviors.Remote {

  val remote = ActorSystem("RemoteFinderSpec-Server", ConfigFactory.load.getConfig("server"))

  override def afterAll() {
    super.afterAll()
    remote.shutdown()
  }

  override protected type Configuration = FinderComponent.Remote.Configuration
  override protected object configuration extends Configuration {
    override val local = RemoteFinderSpec.this.system
    override val remote = "akka.tcp://RemoteFinderSpec-Server@127.0.0.1:2552"
  }

  override protected val actor: FinderComponent.Actor = _actor
  private object _actor extends FinderComponent.Actor {
    override val finder = FinderComponent.Actor.remote(system)(RemoteFinderSpec.this)
  }

  object local extends amber.mock.origin.BuilderComponent.Local
               with FinderComponent.Local
               with amber.origin.FactoryComponent.Local.Default {

    override protected type Origin[+A] = Origin.Local[A]

    override protected val actor: FinderComponent.Actor = _actor
    private object _actor extends FinderComponent.Actor {
      override val finder = FinderComponent.Actor.local(remote)(local.this)
    }

    override def mocker[A](implicit typeA: Type[A]) =
      new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]] {
        def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A])) = {
          val origin = RemoteFinderSpec.this.mock[Origin[A]](s"mock.Origin.Local[$typeA]")
          val name = args._1
          val family = args._2
          val reference = remote.actorOf(Props(new dummy))

          when(origin.name) thenReturn name
          when(origin.family) thenReturn family
          when(origin.returns(anything(), anything())) thenAnswer {
            args: Array[AnyRef] => typeA <:< args(1).asInstanceOf[Type[_]]
          }
          when(origin.writeReplace()) thenAnswer {
            _: Array[AnyRef] =>
              new Origin.Serialized[A](name, family)(reference)
          }

          origin
        }
      }

      private class dummy extends Actor {
        override def receive = {
          case _ => /* ignore */
        }
      }
  }

  override def build[A: NotNothing](name: Origin.Name)(implicit typeA: Type[A]) = {
    val origin = local.origin.create(name)(mock[local.OriginFactory.Read[A]]("Origin.read"))
    Origin.Remote(name, origin.family)(RemoteFinderSpec.this, origin.actor)(typeA, configuration.context, timeout)
  }

  "akka.Remote.OriginFinder" should {
    behave like anOriginFinder
  }
}
