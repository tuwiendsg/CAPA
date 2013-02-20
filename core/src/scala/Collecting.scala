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

import scala.collection.immutable.{Seq, Stream}

import scalaz.syntax.equal._

import util.{Filter, NotNothing}
import util.NotNothing.notNothing

trait Collecting extends origin.BuilderComponent {
  this: origin.FinderComponent with family.MemberFactoryComponent =>

  abstract override protected def builder: OriginBuilder = _builder

  object collect {

    private[amber] val family = Family.random()

    def apply[A: NotNothing : Manifest](name: Property.Name,
                                        filter: Filter[Origin.Meta.Readable]): Stream[Property[A]] =
      for {
        origin <- origins.find(name).toStream
        if (name === origin.name) && origin.returns(notNothing, manifest[A])
        property <- origin.asInstanceOf[Origin[A]].apply(filter)
      } yield property
  }

  private object _builder extends OriginBuilder {
    override def build[A: Manifest, B: Origin.Read[A]#apply](name: Property.Name,
                                                             family: Family,
                                                             read: B) = {
      val origin = Collecting.super.builder.build(name, family, read)
      if (collect.family =/= family) {
        in(collect.family).create[Seq[A]](name) {
          filter =>
            for {values <- Some(collect[A](name, filter)) if !values.isEmpty}
              yield values map {_.value}
        }
      }
      origin
    }
  }
}
