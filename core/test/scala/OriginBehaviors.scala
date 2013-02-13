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

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{never, verify, when}

import util.{Filter, NotNothing}

trait OriginBehaviors {
  this: Spec =>

  def fixture: Fixture

  trait Fixture {

    def create[A: Manifest, B: Origin.Read[A]#apply](name: Origin.Name,
                                                     family: Origin.Family,
                                                     read: B): Origin[A]

    def create[A: NotNothing : Manifest](): Origin[A] = create(random[Origin.Name])

    def create[A: NotNothing : Manifest](name: Origin.Name): Origin[A] =
      create(name, random[Origin.Family])

    def create[A: NotNothing : Manifest](family: Origin.Family): Origin[A] =
      create(random[Origin.Name], family)

    def create[A: NotNothing : Manifest](name: Origin.Name, family: Origin.Family): Origin[A] =
      create(name, family, mock[Origin.Read.Unfiltered[A]]("Origin.read"))

    def create[A: Manifest](read: Origin.Read.Unfiltered[A]): Origin[A] =
      create(random[Origin.Name], random[Origin.Family], read)

    def create[A: Manifest](read: Origin.Read.Filtered[A]): Origin[A] =
      create(random[Origin.Name], random[Origin.Family], read)
  }

  object anOrigin {

    "has the specified name" in {
      val name = random[Origin.Name]
      val origin = fixture.create[Any](name)

      origin.name should be(name)
    }

    "is in the specified family" in {
      val family = random[Origin.Family]
      val origin = fixture.create[Any](family)

      origin.family should be(family)
    }

    "if a meta value was never assigned" should {
      "return None " in {
        val origin = fixture.create[Any]()

        origin.meta[Any](random[Origin.MetaInfo.Name]) should not be('defined)
      }
    }

    "if a meta value was previously assigned" should {
      "return the assigned value" when {
        "requested type is same type as the assigned value" in {
          class A
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin.meta(name) = value

          origin.meta[A](name).value should be(value)
        }

        "requested type is a super type of the assigned value" in {
          class A
          class B extends A
          val name = random[Origin.MetaInfo.Name]
          val value = new B
          val origin = fixture.create[Any]()

          origin.meta(name) = value

          origin.meta[A](name).value should be(value)
        }
      }

      "return None " when {
        "requested type is a different type than the assigned value" in {
          class A
          class B
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin.meta(name) = value

          origin.meta[B](name) should not be('defined)
        }

        "requested type is a sub type of the assigned value" in {
          class A
          class B extends A
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin.meta(name) = value

          origin.meta[B](name) should not be('defined)
        }
      }
    }

    "does return the same type" in {
      class A
      val origin = fixture.create[A]()

      origin.returns[A] should be(true)
    }

    "does return a super type" in {
      class A
      class B extends A
      val origin = fixture.create[B]()

      origin.returns[A] should be(true)
    }

    "does not return a different type" in {
      class A
      class B
      val origin = fixture.create[A]()

      origin.returns[B] should be(false)
    }

    "does not return a sub type" in {
      class A
      class B extends A
      val origin = fixture.create[A]()

      origin.returns[B] should be(false)
    }

    "is created with unfiltered read" should {
      "invoke the specified filter" in {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        val origin = fixture.create(mock[Origin.Read.Unfiltered[_]]("Origin.read"))

        origin(filter)

        verify(filter).apply(anything())
      }

      "if it passes the specified filter" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        when(filter.apply(anything())) thenReturn true

        "use the specified read function" in {
          val read = mock[Origin.Read.Unfiltered[_]]("Origin.read")
          when(read.apply()) thenReturn None
          val origin = fixture.create(read)

          origin(filter)

          verify(read).apply()
        }

        "if reading is successful" should {
          "return a value" which {
            "has the same name as the origin" in {
              val read = mock[Origin.Read.Unfiltered[String]]("Origin.read")
              when(read()) thenReturn Some(random[String])
              val origin = fixture.create(read)

              val Origin.Value(name, _) = origin(filter).value
              name should be(origin.name)
            }

            "contains the result of the specified read function" in {
              val result = random[String]
              val read = mock[Origin.Read.Unfiltered[String]]("Origin.read")
              when(read()) thenReturn Some(result)
              val origin = fixture.create(read)

              val Origin.Value(_, value) = origin(filter).value
              value should be(result)
            }
          }
        }

        "if reading is unsuccessful" should {
          "return None" in {
            val read = mock[Origin.Read.Unfiltered[_]]("Origin.read")
            when(read()) thenReturn None
            val origin = fixture.create(read)

            origin(filter) should not be('defined)
          }
        }
      }

      "if it does not pass the specified filter" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        when(filter.apply(anything())) thenReturn false

        "not invoke the specified read function" in {
          val read = mock[Origin.Read.Unfiltered[_]]("Origin.read")
          val origin = fixture.create(read)

          origin(filter)

          verify(read, never()).apply()
        }

        "return None" in {
          val origin = fixture.create[Any]()

          origin(filter) should not be('defined)
        }
      }
    }

    "is created with filtered read" should {
      "use the specified read function" in {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        val read = mock[Origin.Read.Filtered[_]]("Origin.read")
        when(read.apply(anything())) thenReturn None
        val origin = fixture.create(read)

        origin(filter)

        verify(read).apply(filter)
      }

      "if reading is successful" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")

        "return a value" which {
          "has the same name as the origin" in {
            val read = mock[Origin.Read.Filtered[String]]("Origin.read")
            when(read.apply(anything())) thenReturn Some(random[String])
            val origin = fixture.create(read)

            val Origin.Value(name, _) = origin(filter).value
            name should be(origin.name)
          }

          "contains the result of the specified read function" in {
            val result = random[String]
            val read = mock[Origin.Read.Filtered[String]]("Origin.read")
            when(read.apply(anything())) thenReturn Some(result)
            val origin = fixture.create(read)

            val Origin.Value(_, value) = origin(filter).value
            value should be(result)
          }
        }
      }

      "if reading is unsuccessful" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")

        "return None" in {
          val read = mock[Origin.Read.Filtered[_]]("Origin.read")
          when(read.apply(anything())) thenReturn None
          val origin = fixture.create(read)

          origin(filter) should not be('defined)
        }
      }
    }
  }
}
