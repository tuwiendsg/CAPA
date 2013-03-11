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

import scala.reflect.{classTag, ClassTag}

class Value[+A](value: A) extends Serializable {
  def as[B: NotNothing : ClassTag]: Option[B] = classTag[B] match {
    case ClassTag.Boolean if classOf[java.lang.Boolean].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Boolean].booleanValue.asInstanceOf[B])
    case ClassTag.Byte if classOf[java.lang.Byte].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Byte].byteValue.asInstanceOf[B])
    case ClassTag.Char if classOf[java.lang.Character].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Character].charValue.asInstanceOf[B])
    case ClassTag.Double if classOf[java.lang.Double].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Double].doubleValue.asInstanceOf[B])
    case ClassTag.Float if classOf[java.lang.Float].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Float].floatValue.asInstanceOf[B])
    case ClassTag.Int if classOf[java.lang.Integer].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Integer].intValue.asInstanceOf[B])
    case ClassTag.Long if classOf[java.lang.Long].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Long].longValue.asInstanceOf[B])
    case ClassTag.Short if classOf[java.lang.Short].isInstance(value) =>
      Some(value.asInstanceOf[java.lang.Short].shortValue.asInstanceOf[B])
    case tag if tag.runtimeClass.isInstance(value) => Some(value.asInstanceOf[B])
    case _ => None
  }
}

object Value {

  case class Named[A, +B](name: A, value: B) extends Value[B](value) {
    override lazy val toString = s"$name = $value"
  }

  implicit class OptionValueOperations(val value: Option[Value[_]]) extends AnyVal {
    def as[A: NotNothing : ClassTag]: Option[A] = value flatMap {_.as[A]}
  }
}
