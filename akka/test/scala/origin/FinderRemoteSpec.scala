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
package origin

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.{ActorRef, ActorSystem, Props}

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import amber.util.{NotNothing, Type}

class FinderRemoteSpec extends Spec(ActorSystem("FinderRemoteSpec-Client",
                                    ConfigFactory.load.getConfig("client")))
                       with FinderComponent.Remote
                       with amber.origin.FinderBehaviors.Remote {

  override implicit val context = system.dispatcher
  val remote = ActorSystem("FinderRemoteSpec-Server", ConfigFactory.load.getConfig("server"))
  val actor = remote.actorOf(Props(new FinderComponent.Actor(local)),
                             name = FinderComponent.Actor.name)

  override def afterAll() {
    super.afterAll()
    remote.shutdown()
  }

  override protected type Configuration = FinderComponent.Remote.Configuration
  override protected object configuration extends FinderComponent.Remote.Configuration {
    override val local = FinderRemoteSpec.this.system
    override val remote = "akka://FinderRemoteSpec-Server@127.0.0.1:2552"
  }

  object local extends BuilderComponent with FinderComponent.Local
                                        with amber.origin.FinderComponent.Local.Default
                                        with amber.origin.FactoryComponent.Default

  trait BuilderComponent extends amber.origin.BuilderComponent {

    override protected type Origin[+A] = Origin.Local[A]
    override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      def build[A](name: amber.Origin.Name, family: amber.Origin.Family)
                  (read: OriginBuilder.Read[A])
                  (implicit typeA: Type[A]) = {
        val actor = mock[ActorRef]("mock.ActorRef")
        val origin = mock[Origin.Local[A]](s"mock.akka.Origin.Local[$typeA]")
        when(origin.name) thenReturn name
        when(origin.family) thenReturn family
        when(origin.returns(anything(), anything())) thenAnswer {
          args: Array[AnyRef] => typeA <:< args(1).asInstanceOf[Type[_]]
        }
        when(origin.writeReplace()) thenAnswer {
          _: Array[AnyRef] => new Origin.Serialized[A](name, family)(actor)
        }

        origin
      }
    }
  }

  override val fixture = new Fixture {
    override def create[A: NotNothing](name: amber.Origin.Name)(implicit typeA: Type[A]) = {
      val origin = local.origin.create(name)(mock[local.OriginFactory.Read[A]]("Origin.read"))
      new Origin.Remote(name, origin.family)(origin.actor)(timeout, typeA)
    }
  }

  "akka.Remote.OriginFinder" should {
    behave like aFinder.forOrigins
  }
}
