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

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArraySet}

import scalaz.syntax.std.option._

import scala.collection.immutable.Set
import scala.collection.JavaConversions._

class MultiTrie[A] {

  private val subtries = new ConcurrentHashMap[String, MultiTrie[A]]
  private val values = new CopyOnWriteArraySet[A]

  def add(path: Path, value: A) {
    path match {
      case Path.End => values add value
      case Path.Sub(parent, child) =>
        if (!(subtries containsKey parent)) subtries putIfAbsent (parent, new MultiTrie[A])
        (subtries get parent) add (child, value)
    }
  }

  def find(path: Path): Set[A] = path match {
    case Path.Sub(parent, child) =>
      (Option(subtries get parent) map {_ find child}) | Set.empty[A]
    case Path.End =>
      val builder = Set.newBuilder[A]
      builder ++= values
      for (subtrie <- subtries.values) builder ++= (subtrie find path)
      builder.result()
  }
}

object MultiTrie {
  def apply[A]() = new MultiTrie[A]
}
