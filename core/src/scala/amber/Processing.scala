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

package amber

import scala.language.higherKinds

import scala.collection.immutable.{Seq, Vector}
import scala.concurrent.Future

import scalaz.Id.Id
import scalaz.syntax.equal._
import scalaz.syntax.functor._

import amber.util.{Observer, Type}
import amber.util.Events.observe

trait Processing[X[+_]] {
  this: origin.FinderComponent[X] with origin.FactoryComponent
                                  with family.FinderComponent =>

  object process {

    private var observers = Vector.empty[Observer]

    def apply[A: Type, B: Type]
        (f: PartialFunction[Origin.Name, (Origin.Name, A => B)]): Observer = {
      def onOrigin(source: Origin[A]) {
        if (f isDefinedAt source.name) {
          val (name, g) = f(source.name)
          source.family.synchronized {
            val exists = families.find(source.family) exists {
              origin => (name === origin.name) && origin.returns[B]
            }
            if (!exists) source.map(name)(g)
          }
        }
      }

      val observer = observe(origin.created) {
        case origin if origin.returns[A] => onOrigin(origin.asInstanceOf[Origin[A]])
      }
      synchronized {observers = observers :+ observer}
      origins.find[A](Selections.all) map {for {origin <- _} onOrigin(origin)}
      observer
    }

    private[amber] def shutdown() {
      var old = Vector.empty[Observer]
      synchronized {
        old = observers
        observers = Vector.empty
      }
      for {observer <- old} observer.dispose()
    }
  }

  object map {
    def apply[A: Type, B: Type](input: Origin.Name, output: Origin.Name)(f: A => B): Observer =
      process {case `input` => output -> f}
  }

  object operation {
    def apply[A: Type, B: Type](operation: Operation.Name)(f: A => B): Observer =
      process {case name => (name / operation) -> f}
  }
}

object Processing {

  trait Local extends Processing[Id] {
    this: origin.FinderComponent.Local with origin.FactoryComponent.Local
                                       with family.FinderComponent =>
  }

  trait Remote extends Processing[Future] {
    this: origin.FinderComponent.Remote with origin.FactoryComponent.Remote
                                        with family.FinderComponent =>
  }

  object Default {

    sealed trait Conversions[X[+_]] {
      this: Processing[X] =>

      process[Int, Long] {case name => name -> {x => x.toLong}}
      process[Int, Double] {case name => name -> {x => x.toDouble}}
      process[Long, Double] {case name => name -> {x => x.toDouble}}
    }

    sealed trait Operations[X[+_]] {
      this: Processing[X] =>

      import scala.math.sqrt

      operation[Seq[Int], Int]("min") {xs => xs.min}
      operation[Seq[Int], Int]("max") {xs => xs.max}
      operation[Seq[Int], Long]("sum") {
        xs => xs map {_.longValue} reduceLeft {_ + _}
      }
      operation[Seq[Int], Double]("avg") {
        xs => (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.size
      }
      operation[Seq[Int], Double]("stddev") {
        xs =>
          val average = (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.size
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.size
          sqrt(variance)
      }

      operation[Seq[Long], Long]("min") {xs => xs.min}
      operation[Seq[Long], Long]("max") {xs => xs.max}
      operation[Seq[Long], Long]("sum") {xs => xs reduceLeft {_ + _}}
      operation[Seq[Long], Double]("avg") {
        xs => (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.size
      }
      operation[Seq[Long], Double]("stddev") {
        xs =>
          val average = (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.size
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.size
          sqrt(variance)
      }

      operation[Seq[Double], Double]("min") {xs => xs.min}
      operation[Seq[Double], Double]("max") {xs => xs.max}
      operation[Seq[Double], Double]("sum") {xs => xs reduceLeft {_ + _}}
      operation[Seq[Double], Double]("avg") {xs => (xs reduceLeft {_ + _}) / xs.size}
      operation[Seq[Double], Double]("stddev") {
        xs =>
          val average = (xs reduceLeft {_ + _}) / xs.size
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.size
          sqrt(variance)
      }
    }
  }

  object Local {
    object Default {

      trait Conversions extends Processing.Default.Conversions[Id] {
        this: Processing.Local =>
      }

      trait Operations extends Processing.Default.Operations[Id] {
        this: Processing.Local =>
      }
    }
  }

  object Remote {
    object Default {

      trait Conversions extends Processing.Default.Conversions[Future] {
        this: Processing.Remote =>
      }

      trait Operations extends Processing.Default.Operations[Future] {
        this: Processing.Remote =>
      }
    }
  }
}

object Operation {
  type Name = String
}
