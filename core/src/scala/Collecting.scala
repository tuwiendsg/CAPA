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
import scalaz.syntax.std.option._

import util.{Filter, NotNothing, Observer}
import util.Events.observe
import util.NotNothing.notNothing

trait Collecting {
  this: origin.FinderComponent with origin.FactoryComponent
                               with family.MemberFactoryComponent =>

  protected type Origin[+A <: AnyRef] <: amber.Origin[A]

  val collect = Collect

  object Collect {

    private[amber] val family = Family.random()
    private var observer: Option[Observer] = None

    def apply[A <: AnyRef : NotNothing : Manifest]
        (name: Property.Name, filter: Filter[Origin.Meta.Readable]): Stream[Property[A]] =
      for {
        origin <- origins.find(name).toStream
        if (name === origin.name) && origin.returns(notNothing, manifest[A])
        property <- origin.asInstanceOf[Origin[A]].apply(filter)
      } yield property

    def start() {
      if (!observer.isDefined) {
        synchronized {
          observer = Some {
            observer | observe(origin.created) {
              case (o, manifest) =>
                if (family =/= o.family) {
                  in(family).create(o.name) {
                    filter =>
                      val result: Seq[Property[AnyRef]] = apply(o.name, filter)(notNothing, manifest.asInstanceOf[Manifest[AnyRef]])
                      if (result.isEmpty) None else Some(result map {_.value})
                  }(notNothing, Manifest.classType(classOf[Seq[AnyRef]], manifest))
                }
            }
          }
        }
      }
    }

    def stop() {
      if (observer.isDefined) {
        synchronized {
          observer foreach {_.dispose()}
          observer = None
        }
      }
    }
  }
}
