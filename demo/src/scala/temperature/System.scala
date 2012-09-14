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

import java.lang.{Double, Integer}

import scala.collection.immutable.Seq
import scala.util.Random

trait System extends amber.System {
  object Temperature {

    map[Double, Double]("temperature/celsius", "temperature/kelvin") {x => x + 273.15}
    map[Double, Double]("temperature/celsius", "temperature/fahrenheit") {x => x * 9 / 5 + 3}
    map[Double, Double]("temperature/kelvin", "temperature/celsius") {x => x - 273.15}
    map[Double, Double]("temperature/kelvin", "temperature/fahrenheit") {x => x * 9 / 5 - 459.6}
    map[Double, Double]("temperature/fahrenheit", "temperature/celsius") {x => (x - 32) * 5 / 9}
    map[Double, Double]("temperature/fahrenheit", "temperature/kelvin") {x => (x + 459.67) * 5 / 9}

    def createCelsius(location: String): amber.Origin[Integer] = {
      val temperature = origin.create[Integer]("temperature/celsius") {
        () => Some(Random.nextInt(55) - 15)
      }
      temperature.meta("location") = location
      temperature
    }
  }
}
