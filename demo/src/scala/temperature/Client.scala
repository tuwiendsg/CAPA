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

import scala.language.higherKinds

import scala.concurrent.Future

import scalaz.Functor
import scalaz.Id.{id, Id}
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.applicative._

import util.Value._

sealed trait Client[X[+_]] {
  this: amber.Client[X] with origin.FinderComponent[X] =>

  implicit def X: Functor[X]

  import Selections.exact

  val temperature = entity("Temperature")
  temperature.celsius = {() =>
    readAll[Double](exact("temperature/celsius") where {
      meta => for {location <- meta.location.as[String]} yield location === "A"
    }) map {Some(_)} map {for {values <- _; if !values.isEmpty} yield values.min}
  }
  temperature.kelvin = {() =>
    readAll[Double](exact("temperature/kelvin") where {
      meta => for {location <- meta.location.as[String]} yield location === "B"
    }) map {Some(_)} map {for {values <- _; if !values.isEmpty} yield values.max}
  }
  temperature.where {
    entity =>
      for {
        celsius <- entity.celsius.as[Double]
        kelvin <- entity.kelvin.as[Double]
      } yield celsius < kelvin
  }

  def readTemperature(): X[Option[Entity.Instance]] = readOne(temperature.build())
}

object Client {

  trait Local extends Client[Id] with amber.Client.Local

  trait Remote extends Client[Future] with amber.Client.Remote {
    override protected type Configuration <: Remote.Configuration
  }

  object Remote {
    trait Configuration extends amber.Client.Remote.Configuration
  }
}
