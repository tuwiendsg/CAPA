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

import scala.collection.immutable.{Set, Vector}
import scala.collection.JavaConversions._

import scalaz.Id.Id
import scalaz.iteratee.Iteratee
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.std.vector._
import scalaz.syntax.equal._

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

  def select(selection: MultiTrie.Selection): Set[A] = {
    def step(accumulator: Vector[A])(element: (Path, MultiTrie[A]),
                                     iteratee: Iteratee[(Path, Vector[A]), Vector[A]]): Vector[A] = {
      iteratee.value match {
        case Done(result, _) => accumulator ++ result
        case Cont(next) =>
          val (path, trie) = element
          if (trie.subtries.isEmpty) step(accumulator)(element, next(Eof.apply))
          else trie.subtries.foldLeft(accumulator) {
            case (result, (name, subtrie)) =>
              step(result)((path / name) -> subtrie,
                           next(Element(path / name -> subtrie.values.to[Vector])))
          }
      }
    }

    (selection.iteratee[A].value match {
       case Done(result, _) => result
       case Cont(next) => step(Vector.empty)(Path.Root -> this,
                                             next(Element(Path.Root -> this.values.to[Vector])))
    }).to[Set]
  }
}

object MultiTrie {

  sealed trait Selection extends Serializable {
    def iteratee[A]: Iteratee[(Path, Vector[A]), Vector[A]]
  }

  object Selections {

    case object all extends Selection {
      override def iteratee[A] = takeAll[A]
    }

    case class exact(path: Path) extends Selection {
      override def iteratee[A] = at[A](path)(take)
    }

    case class childrenOf(path: Path) extends Selection {
      override def iteratee[A] = at[A](path)(drop flatMap {_ => take})
    }

    case class descendantsOf(path: Path) extends Selection {
      override def iteratee[A] = at[A](path)(drop flatMap {_ => takeAll})
    }

    case class parentOf(path: Path) extends Selection {
      override def iteratee[A] = path match {
        case Path.Root => done[A]
        case _ =>
          def parent(path: Path): Path = (path: @unchecked) match {
            case Path.Sub(_, Path.End) => Path.Root
            case Path.Sub(name, child) => Path.Sub(name, parent(child))
          }
          at[A](parent(path))(take)
      }
    }

    case class ancestorsOf(path: Path) extends Selection {
      override def iteratee[A] = takeWhile[A] {
        current => (current >:> path) && (current =/= path)
      }
    }

    private def consume[A] = Iteratee.consume[(Path, Vector[A]), Id, Vector]
    private def done[A] = Iteratee.done[(Path, Vector[A]), Id, Vector[A]](Vector.empty, Empty.apply)
    private def head[A] = Iteratee.head[(Path, Vector[A]), Id]
    private def peek[A](f: Path => Iteratee[(Path, Vector[A]), Vector[A]]) =
      Iteratee.peekDoneOr[(Path, Vector[A]), Id, Vector[A]](Vector.empty, {case (path, _) => f(path)})

    private def take[A] = head[A] map {_.fold(Vector.empty[A]) {_._2}}
    private def takeAll[A] = consume[A] map {
      sets => (sets map {case (_, set) => set}).flatten
    }
    private def takeWhile[A](p: Path => Boolean): Iteratee[(Path, Vector[A]), Vector[A]] = peek {
      case path if p(path) => for {current <- take; rest <- takeWhile(p)} yield current ++ rest
      case _ => done
    }

    private def drop[A] = Iteratee.drop[(Path, Vector[A]), Id](1)
    private def dropWhile[A](p: Path => Boolean) = Iteratee.dropWhile[(Path, Vector[A]), Id]{
      case (path, _) => p(path)
    }

    private def at[A]
      (start: Path)(iteratee: Iteratee[(Path, Vector[A]), Vector[A]]) =
      for {
        _ <- dropWhile[A] {path => (path >:> start) && (path =/= start)}
        result <- peek[A] {
          case `start` => iteratee
          case _ => done[A]
        }
      } yield result
  }

  def apply[A]() = new MultiTrie[A]
}
