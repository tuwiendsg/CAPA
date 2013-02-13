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
package origin

import scala.collection.immutable.Set

import util.MultiTrie

trait FinderComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected def origins: OriginFinder

  protected trait OriginFinder {
    def all(): Set[Origin[_]]
    def find(name: Origin.Name): Set[Origin[_]]
  }
}

object FinderComponent {

  trait Default extends FinderComponent with origin.BuilderComponent {

    abstract override protected def builder: OriginBuilder = _builder
    override protected def origins: OriginFinder = _origins

    protected trait OriginFinder extends super.OriginFinder {
      private val trie = MultiTrie[Origin.Name, Origin[_]]()
      override def all() = trie(None)
      override def find(name: Origin.Name) = trie(Some(name))
      def add(origin: Origin[_]) {trie += (Some(origin.name), origin)}
    }

    private object _builder extends OriginBuilder {
      override def build[A: Manifest, B: Origin.Read[A]#apply](name: Origin.Name,
                                                               family: Origin.Family,
                                                               read: B) = {
        val result = Default.super.builder.build(name, family, read)
        origins.add(result)
        result
      }
    }

    private object _origins extends OriginFinder
  }

  trait Delegator extends FinderComponent {

    protected val finder: FinderComponent

    override protected type Origin[+A] = finder.Origin[A]
    override protected object origins extends OriginFinder {
      override def all() = finder.origins.all()
      override def find(name: Origin.Name) = finder.origins.find(name)
    }
  }
}
