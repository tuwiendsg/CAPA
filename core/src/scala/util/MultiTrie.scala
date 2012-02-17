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

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}

import scala.collection.immutable.{Seq, Vector}
import scala.collection.JavaConversions._

import scalaz._
import Scalaz._

class MultiTrie[A : MultiTrie.Key, B] {

  private val subtries = new ConcurrentHashMap[String, MultiTrie[A, B]]
  private val values = new CopyOnWriteArrayList[B]

  def +=(key: Option[A], value: B) {
    key match {
      case MultiTrie.Key(x, xs) =>
        if (subtries get x eq null)
          subtries.putIfAbsent(x, new MultiTrie[A, B])
        subtries get x += (xs, value)
      case _ => values.add(value)
    }
  }

  def apply(key: Option[A]): Seq[B] = key match {
    case MultiTrie.Key(x, xs) =>
      (Option(subtries get x) map {_(xs)}) | Vector.empty
    case _ =>
      val builder = Vector.newBuilder[B]
      builder ++= values
      for (subtrie <- subtries.values) builder ++= subtrie(key)
      builder.result()
  }
}

object MultiTrie {

  trait Key[A] {
    def parts(key: A): (String, Option[A])
  }

  object Key {

    implicit object fromPropertyName extends MultiTrie.Key[Property.Name] {
      override def parts(name: Property.Name) = (name.property, name.child)
    }

    def unapply[A : Key](key: Option[A]): Option[(String, Option[A])] =
      key map {implicitly[Key[A]].parts(_)}
  }

  def apply[K : Key, V]() = new MultiTrie[K, V]
}
