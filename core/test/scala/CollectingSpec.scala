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

import scala.collection.immutable.Seq

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{verify, when}

import util.Filter
import util.Events.observe

class CollectingSpec extends Spec
                     with mock.origin.FinderComponent.WithSuite
                     with mock.origin.FactoryComponent
                     with mock.family.MemberFactoryComponent
                     with Collecting {

  override protected type Origin[+A] = amber.Origin[A]

  trait Fixture {

    class A

    val name = random[Property.Name]
    val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
  }

  override def beforeEach() {
    super.beforeEach()

    collect.start()
    observe(origin.created) {
      case (origin, _) =>
        OriginFinder.add(origin)
        FamilyFinder.add(origin)
    }
  }

  override def afterEach() {
    collect.stop()

    super.afterEach()
  }

  "Collecting" when {
    "an underlying origin is created" should {
      "create an additional origin" which {
        "has same name" in {
          new Fixture {
            origin.create(name)(read)

            families.find(collect.family) should have size(1)
            families.find(collect.family).last.name should be(name)
          }
        }

        "has a sequence of the underlying origin's type for its type" in {
          new Fixture {
            origin.create(name)(read)

            families.find(collect.family) should have size(1)
            families.find(collect.family).last.returns[Seq[A]] should be(true)
          }
        }

        "when invoked" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "invoke the underlying origin" in {
            new Fixture {
              when(read()) thenReturn None
              val underlying = origin.create(name)(read)

              families.find(collect.family) should have size(1)
              families.find(collect.family).last(filter)

              verify(underlying).apply(filter)
            }
          }

          "return a sequence with the result from invoking the underlying origin" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)
              val underlying = origin.create(name)(read)

              families.find(collect.family) should have size(1)
              val result = families.find(collect.family).last(filter).value.asInstanceOf[Property[Set[A]]]

              result.name should be(name)
              result.value should contain(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None
              val underlying = origin.create(name)(read)

              families.find(collect.family) should have size(1)
              val result = families.find(collect.family).last(filter)

              result should not be('defined)
            }
          }
        }
      }
    }

    "the collect method is invoked" should {
      val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
      when(filter.apply(anything())) thenReturn true

      "invoke all origins that have specified name and type" in {
        new Fixture {
          when(read()) thenReturn None
          val originA = origin.create(name)(read)
          val originB = origin.create(name)(read)

          collect[A](name, filter)

          verify(originA).apply(filter)
          verify(originB).apply(filter)
        }
      }

      "return a sequence of results from the invoked origins" in {
        new Fixture {
          val value1 = new A
          val value2 = new A
          when(read()) thenReturn (Some(value1), Some(value2))

          origin.create(name)(read)
          origin.create(name)(read)

          val result = collect[A](name, filter)

          result should have size(2)
          result should contain(Property(name, value1))
          result should contain(Property(name, value2))
        }
      }
    }
  }
}
