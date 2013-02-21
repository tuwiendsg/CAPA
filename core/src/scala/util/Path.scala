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

import scalaz.Equal.equalA

sealed trait Path {
  def >:>(that: Path): Boolean
  def /(end: String): Path
}

object Path {

  val End = Root

  case object Root extends Path {
    override def >:>(that: Path) = true
    override def /(end: String) = Sub(end, End)
    override lazy val toString: String = "/"
  }

  case class Sub(parent: String, child: Path) extends Path {

    override def >:>(that: Path) = that match {
      case Sub(this.parent, thatChild) => this.child >:> thatChild
      case _ => false
    }

    override def /(end: String) = copy(child = child / end)

    override lazy val toString: String = "/" + (child match {
      case End => parent
      case Sub(_, _) => parent + child
    })
  }

  def apply(path: String): Path =
    path.split("/").dropWhile(_.isEmpty).foldLeft[Path](Path.Root) {
      case (parent, child) => parent / child
    }

  implicit def fromString(path: String): Path = apply(path)

  implicit val hasEqual = equalA[Path]
}
