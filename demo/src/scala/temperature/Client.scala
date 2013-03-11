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

trait Client extends amber.Client.Local {

  import Selections.exact

  val temperature = entity("Temperature")
  temperature.celsius = {() =>
    for {
      values <- Some(readAll[Double](exact("temperature/celsius") where {
                  meta => for {location <- meta.location.as[String]} yield location === "A"
                }))
      if !values.isEmpty
    } yield values.min
  }
  temperature.kelvin = {() =>
    for {
      values <- Some(readAll[Double](exact("temperature/kelvin") where {
                  meta => for {location <- meta.location.as[String]} yield location === "B"
                }))
      if !values.isEmpty
    } yield values.max
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
