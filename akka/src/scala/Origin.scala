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

import scala.concurrent.{blocking, future, Future}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}

import _root_.akka.actor.ActorRef
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import scalaz.OptionT
import scalaz.syntax.comonad._

object Origin {

  import Message.{Request, Response}

  abstract class Local[+A: TypeTag](name: amber.Origin.Name, family: amber.Origin.Family)
      extends amber.Origin.Local.Default[A](name, family) {

    def actor: ActorRef

    override lazy val toString = s"akka.Origin.Local[${typeOf[A]}]($name)"
  }

  class Remote[+A](private[akka] val ref: ActorRef)
                  (timeout: Timeout) extends amber.Origin.Remote[A] {

    override def read() = OptionT(request[Response.Read[A]](Request.Read))

    override lazy val name = request[amber.Origin.Name](Request.MetaInfo.Name)
    override lazy val family = request[amber.Origin.Family](Request.MetaInfo.Family)

    override def selectDynamic(name: String) =
      OptionT(request[Response.MetaInfo.Get](Request.MetaInfo.Get(name)))

    override def update[B](name: amber.Origin.MetaInfo.Name, value: B) {
      ref ! Request.MetaInfo.Set(name, value)
    }

    override lazy val hashCode = ref.hashCode

    override def equals(other: Any) = other match {
      case that: akka.Origin.Remote[_] =>
        (that canEqual this) &&
        (this.ref == that.ref)
      case _ => false
    }

    override def canEqual(other: Any) = other.isInstanceOf[akka.Origin.Remote[_]]

    override lazy val toString = s"akka.Origin.Remote($ref)"

    private def request[B: ClassTag](message: Message): Future[B] =
      ask(ref, message)(timeout).mapTo[B]
  }

  class Actor[+A](origin: amber.Origin.Local[A]) extends _root_.akka.actor.Actor {

    import context.dispatcher

    override def receive = {
      case Request.Read => future {blocking {origin.read().run.copoint}} pipeTo sender
      case Request.MetaInfo.Name => sender ! origin.name
      case Request.MetaInfo.Family => sender ! origin.family
      case Request.MetaInfo.Get(name) => sender ! origin.selectDynamic(name).run.copoint
      case set: Request.MetaInfo.Set[_] => set(origin)
    }
  }

  private sealed trait Message extends Serializable

  private object Message {

    object Request {

      case object Read extends Message

      object MetaInfo {
        case object Name extends Message
        case object Family extends Message
        case class Get(name: amber.Origin.MetaInfo.Name) extends Message
        case class Set[+A](name: amber.Origin.MetaInfo.Name, value: A) extends Message {
          def apply(origin: amber.Origin.Local[_]) {origin(name) = value}
        }
      }
    }

    object Response {

      type Read[+A] = Option[(amber.Origin.Value[A], amber.Origin.MetaInfo)]

      object MetaInfo {
        type Get = Option[amber.Origin.MetaInfo.Value[_]]
      }
    }
  }
}
