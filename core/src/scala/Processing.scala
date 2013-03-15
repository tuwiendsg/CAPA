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

import scala.language.higherKinds

import scala.collection.immutable.{Seq, Vector}

import scalaz.syntax.equal._

import util.{Observer, Type}
import util.Events.observe

trait Processing {
  this: origin.FinderComponent.Local with origin.FactoryComponent
                                     with family.MemberFactoryComponent =>

  object process {

    private var observers = Vector.empty[Observer]

    def apply[A: Type, B: Type]
        (f: PartialFunction[Origin.Name, (Origin.Name, A => B)]): Observer = {
      def onOrigin(source: Origin[A]) {
        if (f isDefinedAt source.name) {
          val (name, g) = f(source.name)
          in(source.family).create[B](name) {
            () =>
              for {(Origin.Value(_, value), meta) <- source.read().run} yield (g(value), meta)
          }
        }
      }

      val observer = observe(origin.created) {
        case origin if origin.returns[A] => onOrigin(origin.asInstanceOf[Origin[A]])
      }
      synchronized {observers = observers :+ observer}
      for {origin <- origins.find[A](Selections.all)} onOrigin(origin)
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
      process {
        case name if input === name => output -> f
      }
  }

  object operation {
    def apply[A: Type, B: Type](operation: Operation.Name)(f: A => B): Observer =
      process {
        case name => (name / operation) -> f
      }
  }
}

object Processing {
  object Default {
    trait Conversions {
      this: Processing =>

      process[Int, Long] {case name => name -> {x => x.toLong}}
      process[Int, Double] {case name => name -> {x => x.toDouble}}
      process[Long, Double] {case name => name -> {x => x.toDouble}}
    }

    trait Operations {
      this: Processing =>

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
}

object Operation {
  type Name = String
}
