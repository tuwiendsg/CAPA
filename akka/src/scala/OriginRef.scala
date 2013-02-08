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

import java.util.concurrent.TimeoutException

import _root_.akka.actor.{ActorRef, PoisonPill}
import _root_.akka.dispatch.{Await, Future}
import _root_.akka.pattern.ask
import _root_.akka.util.Timeout

import scalaz.syntax.equal._

import amber.util.{Filter, NotNothing}
import akka.Message.Request.{MetaInfo, Read}

private[akka] case class OriginRef[+A: NotNothing : Manifest](ref: ActorRef)(timeout: Timeout)
    extends amber.Origin[A] {

  override def returns[B: NotNothing : Manifest] = manifest[A] <:< manifest[B]

  override def read(filter: Filter[Origin.Meta.Readable]) =
    await(request[Option[Origin.Value[A]]](Read(filter)))

  override lazy val name = Await.result(request[Origin.Name](MetaInfo.Name), timeout.duration)
  override lazy val family = Await.result(request[Origin.Family](MetaInfo.Family), timeout.duration)

  override def apply[B: NotNothing : Manifest](name: Origin.MetaInfo.Name) =
    await(request[Option[B]](MetaInfo.Get[B](name)))

  override def update[B: Manifest](name: Origin.MetaInfo.Name, value: B) {
    ref ! MetaInfo.Set(name, value)
  }

  private[akka] def kill() {ref ! PoisonPill}

  override lazy val hashCode =
    41 * (41 * (41 + name.hashCode) + family.hashCode) + manifest[A].hashCode

  override def equals(other: Any) = other match {
    case that: Origin[_] =>
      (that canEqual this) &&
      (this.name === that.name) &&
      (this.family === that.family) &&
      that.returns[A]
    case _ => false
  }

  override def canEqual(other: Any) = other.isInstanceOf[Origin[_]]

  override lazy val toString = "akka.Origin[" + manifest[A] + "](" + name + ")"

  private def request[B: Manifest](message: Message): Future[B] =
    ask(ref, message)(timeout).mapTo[B]

  private def await[B](future: Future[Option[B]]): Option[B] =
    try Await.result(future, timeout.duration) catch {case _: TimeoutException => None}
}
