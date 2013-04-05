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

import scala.language.higherKinds

import java.io.ObjectStreamException

import scala.concurrent.{blocking, future, Future}
import scala.reflect.ClassTag

import _root_.akka.actor.ActorRef
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import scalaz.OptionT
import scalaz.syntax.equal._

import amber.util.{NotNothing, Type}

object Origin {

  abstract class Local[+A](name: amber.Origin.Name, family: amber.Origin.Family)
                          (implicit typeA: Type[A])
      extends amber.Origin.Local.Default[A](name, family) with java.io.Serializable {

    private[akka] def actor: ActorRef

    override lazy val toString = s"akka.Origin.Local[$typeA]($name, $family)"

    @throws[ObjectStreamException] def writeReplace(): AnyRef =
      new Origin.Serialized[A](name, family)(actor)
  }

  class Remote[+A](override val name: amber.Origin.Name, override val family: amber.Origin.Family)
                  (reference: ActorRef)
                  (implicit timeout: Timeout, typeA: Type[A])
      extends amber.Origin.Remote[A] with java.io.Serializable {

    import Message.{Request, Response}

    override def returns[B: NotNothing](implicit typeB: Type[B]) = typeA <:< typeB
    override def read() = OptionT(request[Response.Reading[A]](Request.Read))

    override def selectDynamic(name: String) =
      OptionT(request[Response.MetaInfo.Get](Request.MetaInfo.Get(name)))

    override def update[B: Type](name: amber.Origin.MetaInfo.Name, value: B) {
      reference ! Request.MetaInfo.Set(name, value)
    }

    override lazy val hashCode = 41 * (41 + name.hashCode) + family.hashCode

    override def equals(other: Any) = other match {
      case that: amber.Origin[_] =>
        (that canEqual this) &&
        (this.name === that.name) &&
        (this.family === that.family) &&
        that.returns[A]
      case _ => false
    }

    override def canEqual(other: Any) = other.isInstanceOf[Origin[_]] &&
                                        other.asInstanceOf[Origin[_]].returns[A]

    override lazy val toString = s"akka.Origin.Remote[$typeA]($name, $family)"

    @throws[ObjectStreamException] def writeReplace(): AnyRef =
      new Origin.Serialized[A](name, family)(reference)

    private def request[B: ClassTag](message: Message) = ask(reference, message)(timeout).mapTo[B]
  }

  class Actor[+A](origin: amber.Origin.Local[A]) extends _root_.akka.actor.Actor {

    import Message.Request
    import context.dispatcher

    override def receive = {
      case Request.Read => future {blocking {origin.read()}} pipeTo sender
      case Request.MetaInfo.Get(name) => sender ! origin.selectDynamic(name)
      case set: Request.MetaInfo.Set[_] => set(origin)
    }
  }

  class Serialized[A](name: amber.Origin.Name, family: amber.Origin.Family)
                     (reference: ActorRef)(implicit typeA: Type[A]) extends java.io.Serializable {

    def returns[B: NotNothing](implicit typeB: Type[B]): Boolean = typeA <:< typeB

    def toRemote(implicit timeout: Timeout): Origin.Remote[A] =
      new Origin.Remote[A](name, family)(reference)
  }

  private sealed trait Message extends Serializable

  private object Message {

    object Request {

      case object Read extends Message

      object MetaInfo {
        case class Get(name: amber.Origin.MetaInfo.Name) extends Message
        case class Set[+A: Type](name: amber.Origin.MetaInfo.Name, value: A) extends Message {
          def apply(origin: amber.Origin.Local[_]) {origin(name) = value}
        }
      }
    }

    object Response {

      type Reading[+A] = amber.Origin.Local.Reading[(amber.Origin.Value[A], amber.Origin.MetaInfo)]

      object MetaInfo {
        type Get = Option[amber.Origin.MetaInfo.Value[_]]
      }
    }
  }
}
