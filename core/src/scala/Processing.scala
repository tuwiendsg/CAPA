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

import scala.collection.immutable.{Seq, Vector}

import util.Observer
import util.Events.observe

trait Processing {
  this: origin.FactoryComponent with family.MemberFactoryComponent =>

  protected type Origin[+A <: AnyRef] <: amber.Origin[A]
  val process = ProcessorFactory
  val map = MapperFactory
  val operation = OperationFactory

  object ProcessorFactory {

    private var observers = Vector.empty[Observer]

    def apply[A <: AnyRef : Manifest, B <: AnyRef : Manifest]
        (f: PartialFunction[Property.Name, (Property.Name, A => B)]): Observer = {
      val observer = observe(origin.created) {
        case (o, _) if o.returns[A] =>
          val source = o.asInstanceOf[Origin[A]]
          if (f isDefinedAt source.name) {
            val (name, g) = f(source.name)
            in(source.family).create(name) {
              filter => source(filter) map {property => g(property.value)}
            }
          }
      }
      synchronized {observers = observers :+ observer}
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

  object MapperFactory {
    def apply[A <: AnyRef : Manifest, B <: AnyRef : Manifest]
        (input: Property.Name, output: Property.Name)(f: A => B): Observer =
      process {
        case name if input == name => output -> f
      }
  }

  object OperationFactory {
    def apply[A <: AnyRef : Manifest, B <: AnyRef : Manifest]
        (operation: Operation.Name)(f: A => B): Observer =
      process {
        case name => (name / operation) -> f
      }
  }
}

object Processing {
  object Default {
    trait Conversions {
      this: Processing =>

      import java.lang.{Double, Integer, Long}

      process[Integer, Long] {case name => name -> {x => x.toLong}}
      process[Integer, Double] {case name => name -> {x => x.toDouble}}
      process[Long, Double] {case name => name -> {x => x.toDouble}}
    }

    trait Operations {
      this: Processing =>

      import java.lang.{Double, Integer, Long}
      import scala.math.sqrt

      operation[Seq[Integer], Integer]("min") {xs => xs.min}
      operation[Seq[Integer], Integer]("max") {xs => xs.max}
      operation[Seq[Integer], Long]("sum") {
        xs => xs map {_.longValue} reduceLeft {_ + _}
      }
      operation[Seq[Integer], Double]("avg") {
        xs => (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.length
      }
      operation[Seq[Integer], Double]("stddev") {
        xs =>
          val average = (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.length
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.length
          sqrt(variance)
      }

      operation[Seq[Long], Long]("min") {xs => xs.min}
      operation[Seq[Long], Long]("max") {xs => xs.max}
      operation[Seq[Long], Long]("sum") {xs => xs reduceLeft {_ + _}}
      operation[Seq[Long], Double]("avg") {
        xs => (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.length
      }
      operation[Seq[Long], Double]("stddev") {
        xs =>
          val average = (xs map {_.doubleValue} reduceLeft {_ + _}) / xs.length
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.length
          sqrt(variance)
      }

      operation[Seq[Double], Double]("min") {xs => xs.min}
      operation[Seq[Double], Double]("max") {xs => xs.max}
      operation[Seq[Double], Double]("sum") {xs => xs reduceLeft {_ + _}}
      operation[Seq[Double], Double]("avg") {xs => (xs reduceLeft {_ + _}) / xs.length}
      operation[Seq[Double], Double]("stddev") {
        xs =>
          val average = (xs reduceLeft {_ + _}) / xs.length
          val variance = (xs.foldLeft(0D) {
            (result, x) =>
            val diff = (x - average)
            result + diff * diff
          }) / xs.length
          sqrt(variance)
      }
    }
  }
}

object Operation {
  type Name = String
}
