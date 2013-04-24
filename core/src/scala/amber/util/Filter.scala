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
package util

import scala.util.Try

class Filter[-A](conditional: A => Boolean, default: Boolean) extends (A => Boolean) {
  def apply(a: A) = Try {conditional(a)} getOrElse default
}

object Filter {

  val tautology: Filter[Any] = Filter(conditional = {_ => true}, default = true)

  def apply[A](conditional: A => Boolean, default: Boolean): Filter[A] =
    new Filter(conditional, default)
}

trait Filterable[A, B] {

  protected val filterDefault = false

  def where(filter: Filter[A]): B
  def where(conditional: A => Boolean): B = where(Filter(conditional, filterDefault))
}
