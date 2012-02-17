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

import scalaz._
import Scalaz._

class Filter[-A](conditional: A => Option[Boolean], default: Boolean) extends (A => Boolean) {
  def apply(a: A) = conditional(a) | default
}

object Filter {

  val tautology = Filter[Any](_ => None, true)

  def apply[A](conditional: A => Option[Boolean], default: Boolean) =
    new Filter(conditional, default)
}

trait Filterable[A, B] {

  protected val filterDefault = false

  def where(filter: Filter[A]): B
  def where(conditional: A => Option[Boolean]): B = where(Filter(conditional, filterDefault))
}
