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

  import Randoms.Manifest

  def different[A: Equal : Manifest](value: A): A = random[A](value =/= (_: A))

  def random[A: Manifest](p: A => Boolean): A = {
    Stream.continually(random[A]).dropWhile(!p(_)).head
  }

  def random[A: NotNothing : Manifest]: A = randomFor(manifest[A]).asInstanceOf[A]

  private def randomFor(m: Manifest[_]): Any = m match {
    case Manifest.Family => Family.random()
    case Manifest.PropertyName => Property.Name(random((_: String) forall {_ =/= '/'}))
    case Manifest.String => new String(alphanumeric.take(10).toArray)
  }
}

object Randoms {
  private object Manifest {
    val Family = manifest[Family]
    val PropertyName = manifest[Property.Name]
    val String = manifest[String]
  }
}
