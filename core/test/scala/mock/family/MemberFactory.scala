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
package mock.family

import java.util.concurrent.ConcurrentHashMap

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import org.scalatest.Suite
import org.scalatest.mock.MockitoSugar.mock

import util.Mocking
import util.NotNothing.notNothing

trait MemberFactoryComponent extends amber.family.MemberFactoryComponent
                             with FinderComponent.WithSuite
                             with Mocking {
  this: Suite =>

  override protected type Origin[+A <: AnyRef] = amber.Origin[A]

  private val factories = new ConcurrentHashMap[Family, MemberFactory]
  override def in(family: Family) = {
    if (!factories.containsKey(family)) {
      val factory = mock[MemberFactory]("mock.MemberFactory(" + family + ")")
      when(factory.create(anything())(anything())(anything(), anything())) thenAnswer {
        args: Array[AnyRef] =>
          val name = args(0).asInstanceOf[Property.Name]
          val read = args(1).asInstanceOf[Origin.Read.Filtered[AnyRef]]
          val manifest = args(3).asInstanceOf[Manifest[AnyRef]]

          family.synchronized {
            val exists = families.find(family) exists {
              origin => (name == origin.name) && origin.returns(notNothing, manifest)
            }
            if (exists) None
            else {
              val origin = amber.mock.Origin.create(name, family, read)(notNothing, manifest)
              FamilyFinder.add(origin)
              Some(origin)
            }
          }
      }

      factories.putIfAbsent(family, factory)
    }

    factories.get(family)
  }
}
