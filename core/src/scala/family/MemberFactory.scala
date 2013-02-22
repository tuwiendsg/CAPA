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

import scalaz.syntax.equal._

import util.Logger

trait MemberFactoryComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected def in(family: Family): MemberFactory

  protected trait MemberFactory {
    def create[A: Manifest](name: Origin.Name)(read: Origin.Read.Filtered[A]): Option[Origin[A]]
  }

  protected object MemberFactory {
    trait Logging extends MemberFactory {

      protected def log: Logger

      abstract override def create[A: Manifest](name: Origin.Name)
                                               (read: Origin.Read.Filtered[A]) = {
        val result = super.create(name)(read)
        if (result.isDefined) log.debug("Created " + name + " origin of type " + manifest[A])
        result
      }
    }
  }
}

object MemberFactoryComponent {
  trait Default extends MemberFactoryComponent {
    this: origin.BuilderComponent with FinderComponent =>

    override protected def in(f: Family) = new MemberFactory {
      override protected val family = f
    }

    protected trait MemberFactory extends super.MemberFactory {

      protected def family: Family

      override def create[A: Manifest](name: Origin.Name)(read: Origin.Read.Filtered[A]) =
        family.synchronized {
          val exists = families.find(family) exists {
            origin => (name === origin.name) && origin.returns[A]
          }
          if (exists) None
          else Some(builder.build(name, family, read))
        }
    }
  }
}
