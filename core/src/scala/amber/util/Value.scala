/*
 * Copyright 2013 Sanjin Sehic
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

class Value[+A](value: A)(implicit typeA: Type[A]) extends Serializable {

  def as[B: NotNothing](implicit typeB: Type[B]): Option[B] =
    if (typeA <:< typeB) Some(value.asInstanceOf[B]) else None

  def map[B: Type](f: A => B): Value[B] = new Value[B](f(value))

  override lazy val toString = s"Value[$typeA]($value)"
}

object Value {

  case class Named[A, +B: Type](name: A, value: B) extends Value[B](value) {
    override def map[C: Type](f: B => C): Named[A, C] = Named[A, C](name, f(value))
    override lazy val toString = s"$name = $value"
  }

  implicit class OptionValueOperations(val value: Option[Value[_]]) extends AnyVal {
    def as[A: NotNothing : Type]: Option[A] = value flatMap {_.as[A]}
  }
}
