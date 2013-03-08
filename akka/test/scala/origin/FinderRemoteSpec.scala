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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}

import _root_.akka.actor.{ActorRef, Props}

import org.mockito.Mockito.when

class FinderRemoteSpec extends Spec("FinderRemoteSpec")
                       with FinderComponent.Remote
                       with amber.origin.FinderBehaviors.Remote {

  override implicit val context = system.dispatcher

  override protected type Configuration = FinderComponent.Remote.Configuration
  override protected object configuration extends FinderComponent.Remote.Configuration {
    override val system = FinderRemoteSpec.this.system
  }

  object local extends BuilderComponent with FinderComponent.Local
                                        with amber.origin.FinderComponent.Local.Default
                                        with amber.origin.FactoryComponent.Default

  trait BuilderComponent extends amber.origin.BuilderComponent {

    override protected type Origin[+A] = Origin.Local[A]
    override protected def builder: OriginBuilder = _builder

    private object _builder extends OriginBuilder {
      def build[A: ClassTag : TypeTag](name: amber.Origin.Name, family: amber.Origin.Family)
                                      (read: OriginBuilder.Read[A]) = {
        val origin = mock[Origin.Local[A]](s"mock.akka.Origin.Local[${typeOf[A]}]")
        when(origin.name) thenReturn name
        when(origin.actor) thenReturn mock[ActorRef](s"mock.ActorRef")
        origin
      }
    }
  }

  val actor = system.actorOf(Props(new FinderComponent.Actor(local)), name="origins-finder")

  override val fixture = new Fixture {
    override def create(name: amber.Origin.Name) =
      new Origin.Remote[AnyRef](
          local.origin.create(name)(mock[local.OriginFactory.Read[AnyRef]]("Origin.read")).actor
      )(timeout)
  }

  "akka.Remote.OriginFinder" should {
    behave like aFinder.forOrigins
  }
}
