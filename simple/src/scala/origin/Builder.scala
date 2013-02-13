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
package simple
package origin

import amber.util.{Filter, Logging}

trait BuilderComponent extends amber.origin.BuilderComponent {
  this: Logging =>

  override protected type Origin[+A] = simple.Origin[A]
  override protected def builder: super.OriginBuilder = _builder

  protected trait OriginBuilder extends super.OriginBuilder {
    override def build[A: Manifest, B: Origin.Read[A]#apply](name: Origin.Name,
                                                             family: Origin.Family,
                                                             read: B) = {
      val log = logger.create("amber.simple.Origin(" + name + ")")
      def process(result: Option[A]): Option[Origin.Value[A]] =
        for (value <- result) yield {
          log.debug("Read " + value + " from " + name)
          Origin.Value(name, value)
        }

      read match {
        case f: Origin.Read.Unfiltered[A] =>
          new Origin(name, family) {
            override def read(filter: Filter[Origin.Meta.Readable]) =
              if (filter(meta)) process(f()) else None
          }
        case f: Origin.Read.Filtered[A] =>
          new Origin(name, family) {
            override def read(filter: Filter[Origin.Meta.Readable]) = process(f(filter))
          }
      }
    }
  }

  private object _builder extends OriginBuilder
}
