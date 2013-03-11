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

import scala.language.higherKinds

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArraySet}

import scala.collection.immutable.Set
import scala.collection.JavaConversions._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait FinderComponent {

  protected type Origin[+A] <: amber.Origin.Local[A]
  protected def families: FamilyFinder

  protected trait FamilyFinder {
    def find(family: Origin.Family): Set[Origin[_]]
  }
}

object FinderComponent {

  trait Default extends FinderComponent with origin.BuilderComponent {

    abstract override protected def builder: OriginBuilder = _builder
    override protected def families: FamilyFinder = _families

    protected trait FamilyFinder extends super.FamilyFinder {

      private val families = new ConcurrentHashMap[Origin.Family, CopyOnWriteArraySet[Origin[_]]]

      override def find(family: Origin.Family) =
        Option(families.get(family)).fold(Set.empty[Origin[_]]) {_.toSet}

      def add(origin: Origin[_]) {
        val family = origin.family
        if (families get family eq null)
          families.putIfAbsent(family, new CopyOnWriteArraySet[Origin[_]]())
        families.get(family).add(origin)
      }
    }

    private object _builder extends OriginBuilder {
      override def build[A: ClassTag : TypeTag](name: Origin.Name, family: Origin.Family)
                                               (read: OriginBuilder.Read[A]) = {
        val result = Default.super.builder.build(name, family)(read)
        families.add(result)
        result
      }
    }

    private object _families extends FamilyFinder
  }

  trait Delegator extends FinderComponent {

    protected val finder: FinderComponent

    override protected type Origin[+A] = finder.Origin[A]
    override protected object families extends FamilyFinder {
      override def find(family: Origin.Family) = finder.families.find(family)
    }
  }
}
