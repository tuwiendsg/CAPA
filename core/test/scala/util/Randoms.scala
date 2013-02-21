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
package util

import scala.collection.immutable.Stream
import scala.util.Random.alphanumeric

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.syntax.equal._

trait Randoms {

  def different[A: Equal : Random](value: A): A = random[A](value =/= (_: A))
  def random[A: Random](p: A => Boolean): A = {Stream.continually(random[A]).dropWhile(!p(_)).head}
  def random[A](implicit random: Random[A]): A = random.instance()

  trait Random[A] {
    def instance(): A
  }

  implicit object StringHasRandom extends Random[String] {
    override def instance() = new String(alphanumeric.take(10).toArray)
  }

  implicit object PropertyNameHasRandom extends Random[Property.Name] {
    override def instance() = Property.Name(random((_: String) forall {_ =/= '/'}))
  }

  implicit object FamilyHasRandom extends Random[Family] {
    override def instance() = Family.random()
  }
}
