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

import scala.language.higherKinds

import java.io.ObjectStreamException

import scala.concurrent.{blocking, future, ExecutionContext, Future}
import scala.reflect.ClassTag

import _root_.akka.actor.ActorRef
import _root_.akka.pattern.{ask, pipe}
import _root_.akka.util.Timeout

import scalaz.OptionT
import scalaz.syntax.equal._

import amber.origin.BuilderComponent.OriginOps
import amber.util.{NotNothing, Type}

sealed trait Origin[+A] extends java.io.Serializable {
  this: amber.Origin[A] =>

  private[akka] def actor: ActorRef
  protected def write(): Origin.Serialized[A]

  @throws[ObjectStreamException] def writeReplace(): AnyRef = write()
}

object Origin {

  type Name = amber.Origin.Name
  type Family = amber.Origin.Family
  type Value[+A] = amber.Origin.Value[A]
  type MetaInfo = amber.Origin.MetaInfo

  val Value = amber.Origin.Value
  val MetaInfo = amber.Origin.MetaInfo

  abstract class Local[+A](name: Origin.Name, family: Origin.Family)(implicit typeA: Type[A])
      extends Origin.Local.Default[A](name, family) with Origin[A] {

    override def map[B: Type](name: Origin.Name)(f: A => B): Origin.Local[B]

    override lazy val toString = s"akka.Origin.Local[$typeA]($name, $family)"

    override protected def write() = new Origin.Serialized[A](name, family)(actor)
  }

  val Local = amber.Origin.Local

  abstract class Remote[+A](override val name: Origin.Name, override val family: Origin.Family)
                           (implicit timeout: Timeout, typeA: Type[A])
      extends amber.Origin.Remote[A] with Origin[A] {

    override def map[B: Type](name: Origin.Name)(f: A => B): Origin.Remote[B]

    override def returns[B: NotNothing](implicit typeB: Type[B]) = typeA <:< typeB

    override lazy val hashCode = 41 * (41 + name.hashCode) + family.hashCode

    override def equals(other: Any) = other match {
      case that: amber.Origin[_] =>
        (that canEqual this) &&
        (this.name === that.name) &&
        (this.family === that.family) &&
        that.returns[A]
      case _ => false
    }

    override def canEqual(other: Any) = other.isInstanceOf[amber.Origin[_]] &&
                                        other.asInstanceOf[amber.Origin[_]].returns[A]

    override lazy val toString = s"akka.Origin.Remote[$typeA]($name, $family)"

    override protected def write() = new Origin.Serialized[A](name, family)(actor)
  }

  object Remote {

    type Reading[+A] = amber.Origin.Remote.Reading[A]

    def apply[A: Type](name: Origin.Name, family: Origin.Family)
                      (builder: origin.BuilderComponent.Remote, reference: ActorRef)
                      (implicit context: ExecutionContext, timeout: Timeout): Origin.Remote[A] =
      new Origin.Remote[A](name, family) with OriginOps[A] {

        import Message.{Request, Response}

        override private[akka] val actor = reference

        override def read() = OptionT(request[Response.Reading[A]](Request.Read))

        override def selectDynamic(name: String) =
          OptionT(request[Response.MetaInfo.Get](Request.MetaInfo.Get(name)))

        override def update[B: Type](name: Origin.MetaInfo.Name, value: B) {
          reference ! Request.MetaInfo.Set(name, value)
        }

        override def map[B: Type](name: Origin.Name)(f: A => B) = map(builder, name)(f)

        private def request[B: ClassTag](message: Message) =
          ask(reference, message)(timeout).mapTo[B]
      }
  }

  object Actor {

    import Message.{Request, Response}

    class Local[+A](origin: amber.Origin.Local[A]) extends _root_.akka.actor.Actor {

      import context.dispatcher

      override def receive = {
        case Request.Read => future {blocking {origin.read()}} pipeTo sender
        case Request.MetaInfo.Get(name) => sender ! origin.selectDynamic(name)
        case set: Request.MetaInfo.Set[_] => set(origin)
      }
    }

    class Remote[+A](origin: amber.Origin.Remote[A]) extends _root_.akka.actor.Actor {

      import context.dispatcher

      override def receive = {
        case Request.Read => origin.read().run pipeTo sender
        case Request.MetaInfo.Get(name) => origin.selectDynamic(name).run pipeTo sender
        case set: Request.MetaInfo.Set[_] => set(origin)
      }
    }
  }

  class Serialized[+A](name: Origin.Name, family: Origin.Family)
                     (reference: ActorRef)(implicit typeA: Type[A]) extends java.io.Serializable {

    def returns[B: NotNothing](implicit typeB: Type[B]): Boolean = typeA <:< typeB

    def toRemote(builder: origin.BuilderComponent.Remote)
                (implicit context: ExecutionContext, timeout: Timeout): Origin.Remote[A] =
      Origin.Remote[A](name, family)(builder, reference)

    override lazy val toString = s"akka.Origin.Serialized[$typeA]($name, $family)"
  }

  private sealed trait Message extends Serializable

  private object Message {

    object Request {

      case object Read extends Message

      object MetaInfo {
        case class Get(name: Origin.MetaInfo.Name) extends Message
        case class Set[+A: Type](name: Origin.MetaInfo.Name, value: A) extends Message {
          def apply(origin: amber.Origin[_]) {origin(name) = value}
        }
      }
    }

    object Response {

      type Reading[+A] = Origin.Local.Reading[(Origin.Value[A], Origin.MetaInfo)]

      object MetaInfo {
        type Get = Option[Origin.MetaInfo.Value[_]]
      }
    }
  }
}
