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

import _root_.akka.actor.{ActorRef, PoisonPill}

import scalaz.syntax.equal._

import amber.Origin.{Meta, MetaInfo}
import amber.util.{Filter, NotNothing}
import Message.Request

private[akka] case class OriginRef[+A <: AnyRef : Manifest]
    (override val name: Property.Name, override val family: Family)
    (ref: ActorRef) extends amber.Origin[A] {

  override def apply(filter: Filter[Meta.Readable]) =
    (ref ? Request.Value(filter)).as[Option[Property[A]]] flatMap {identity}

  override def returns[B <: AnyRef : NotNothing : Manifest] = manifest[A] <:< manifest[B]

  override val meta = new Meta.Writable {

    override def apply[B <: AnyRef : NotNothing : Manifest](name: MetaInfo.Name) =
      (ref ? Request.MetaInfo.Get[B](name)).as[Option[B]] flatMap {identity}

    override def update[B <: AnyRef : Manifest](name: MetaInfo.Name, value: B) {
      ref ! Request.MetaInfo.Set(name, value)
    }
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
}
