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
package family

import scala.language.higherKinds

import scalaz.syntax.equal._

import util.{Logger, Type}

trait MemberFactoryComponent {

  protected type Origin[+A] <: Origin.Local[A]
  protected def in(family: Origin.Family): MemberFactory

  protected trait MemberFactory {
    def create[A: Type](name: Origin.Name)(read: MemberFactory.Read[A]): Option[Origin[A]]
  }

  protected object MemberFactory {

    type Read[+A] = () => Origin.Local.Reading[(A, Origin.MetaInfo)]

    trait Logging extends MemberFactory {

      protected def log: Logger

      abstract override def create[A](name: Origin.Name)(read: MemberFactory.Read[A])
                                     (implicit typeA: Type[A]) = {
        val result = super.create(name)(read)
        if (result.isDefined) log.debug(s"Created $name origin of type $typeA")
        result
      }
    }
  }
}

object MemberFactoryComponent {
  trait Default extends MemberFactoryComponent {
    this: origin.BuilderComponent.Local with FinderComponent =>

    override protected def in(f: Origin.Family) = new MemberFactory {
      override protected val family = f
    }

    protected trait MemberFactory extends super.MemberFactory {

      protected def family: Origin.Family

      override def create[A: Type](name: Origin.Name)(read: MemberFactory.Read[A]) =
        family.synchronized {
          val exists = families.find(family) exists {
            origin => (name === origin.name) && origin.returns[A]
          }
          if (exists) None
          else Some(builder.build(name, family) {
            meta => read() map {case (a, other) => (Origin.Value(name, a), meta :+ other)}
          })
        }
    }
  }
}
