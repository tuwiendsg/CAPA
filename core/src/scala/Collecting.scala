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

    private[amber] val family = Origin.Family.random()

    def apply[A: NotNothing : Manifest]
        (name: Origin.Name, filter: Filter[Origin.Meta.Readable]): Stream[Origin.Value[A]] =
      for {
        origin <- origins.find(Selections.exact(name)).toStream
        if origin.returns(notNothing, manifest[A])
        value <- origin.asInstanceOf[Origin[A]].read(filter)
      } yield value
  }

  private object _builder extends OriginBuilder {
    override def build[A: Manifest, B: Origin.Read[A]#apply](name: Origin.Name,
                                                             family: Origin.Family,
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
