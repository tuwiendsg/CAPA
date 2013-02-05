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

import scala.collection.immutable.Set

import util.Scheduling

trait Demo extends Runnable with Scheduling {

  def system: temperature.System
  def client: temperature.Client

  protected val delimiter = "-" * 40
  protected val locations = Set("A", "B")
  protected val origins = 5

  override def run() {
    try {
      for {location <- locations; _ <- 1 to origins} system.Temperature.createCelsius(location)
      println(delimiter)
      every(2 seconds) {() => client.printTemperature(); println(delimiter)}
      readLine()
    } finally {
      shutdown()
      system.shutdown()
    }
  }
}
