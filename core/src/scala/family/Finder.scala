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
package family

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArraySet}

import scala.collection.immutable.Set
import scala.collection.JavaConversions._

import scalaz.syntax.std.option._

import util.NotNothing

trait FinderComponent {

  protected type Origin[+A <: AnyRef] <: amber.Origin[A]
  protected def families: FamilyFinder

  protected trait FamilyFinder {
    def all(): Set[Origin[_ <: AnyRef]]
    def find(name: Family): Set[Origin[_ <: AnyRef]]
  }
}

object FinderComponent {

  trait Default extends FinderComponent with origin.BuilderComponent {

    abstract override protected def builder: OriginBuilder = new Wrapper(super.builder)
    override protected def families: FamilyFinder = _families

    protected trait FamilyFinder extends super.FamilyFinder {

      private val families = new ConcurrentHashMap[Family, CopyOnWriteArraySet[Origin[_ <: AnyRef]]]

      override def all() = {
        val builder = Set.newBuilder[Origin[_ <: AnyRef]]
        for ((_, family) <- families) builder ++= family
        builder.result()
      }

      override def find(family: Family) =
        (for {origins <- Option(families.get(family))} yield origins.toSet) | Set.empty

      def add(origin: Origin[_ <: AnyRef]) {
        val family = origin.family
        if (families get family eq null)
          families.putIfAbsent(family, new CopyOnWriteArraySet[Origin[_ <: AnyRef]]())
        families.get(family).add(origin)
      }
    }

    private class Wrapper(underlying: OriginBuilder) extends OriginBuilder {
      override def build[A <: AnyRef : NotNothing : Manifest, B: Origin.Read[A]#apply]
          (name: Property.Name, family: Family, read: B) = {
        val result = underlying.build(name, family, read)
        families.add(result)
        result
      }
    }

    private object _families extends FamilyFinder
  }

  trait Delegator extends FinderComponent {

    protected val delegatee: FinderComponent

    override protected type Origin[+A <: AnyRef] = delegatee.Origin[A]
    override protected def families: super.FamilyFinder = _families

    protected trait FamilyFinder extends super.FamilyFinder {
      override def all() = delegatee.families.all()
      override def find(name: Family) = delegatee.families.find(name)
    }

    private object _families extends FamilyFinder
  }
}
