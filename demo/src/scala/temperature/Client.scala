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
package demo
package temperature

import scalaz.std.string._
import scalaz.syntax.equal._

trait Client extends amber.Client {

  import Selections.exact

  val temperature = entity("Temperature")
  temperature.has[Double].celsius = exact("temperature/celsius/min") where {
    meta => for {location <- meta.location.as[String]} yield location === "A"
  }
  temperature.has[Double].kelvin = exact("temperature/kelvin/max") where {
    meta => for {location <- meta.location.as[String]} yield location === "B"
  }
  temperature.where {
    entity =>
      for {
        celsius <- entity.celsius.as[Double]
        kelvin <- entity.kelvin.as[Double]
      } yield celsius < kelvin
  }

  def readTemperature(): Option[Entity.Instance] = readOne(temperature.build())
}
