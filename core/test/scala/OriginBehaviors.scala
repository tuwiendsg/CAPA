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

import Origin.{Meta, MetaInfo}
import util.{Filter, NotNothing, Randoms}

trait OriginBehaviors {
  this: Spec =>

  object AnOrigin {

    def withName(fixture: OriginBehaviors.Fixture.WithNoRead) {
      "has the specified name" in {
        val name = random[Property.Name]
        val origin = fixture.create[AnyRef](name = name)

        origin.name should be(name)
      }
    }

    def withFamily(fixture: OriginBehaviors.Fixture.WithNoRead) {
      "is in the specified family" in {
        val family = random[Family]
        val origin = fixture.create[AnyRef](family = family)

        origin.family should be(family)
      }
    }

    def withMetaInfo(fixture: OriginBehaviors.Fixture.WithNoRead) {
      "if a meta value was never assigned" should {
        "return None " in {
          val origin = fixture.create[AnyRef]()

          origin.meta[AnyRef](random[MetaInfo.Name]) should not be('defined)
        }
      }

      "if a meta value was previously assigned" should {
        "return the assigned value" when {
          "requested type is same type as the assigned value" in {
            class A
            val name = random[MetaInfo.Name]
            val value = new A
            val origin = fixture.create[AnyRef]()

            origin.meta(name) = value

            origin.meta[A](name).value should be(value)
          }

          "requested type is a super type of the assigned value" in {
            class A
            class B extends A
            val name = random[MetaInfo.Name]
            val value = new B
            val origin = fixture.create[AnyRef]()

            origin.meta(name) = value

            origin.meta[A](name).value should be(value)
          }
        }

        "return None " when {
          "requested type is a different type than the assigned value" in {
            class A
            class B
            val name = random[MetaInfo.Name]
            val value = new A
            val origin = fixture.create[AnyRef]()

            origin.meta(name) = value

            origin.meta[B](name) should not be('defined)
          }

          "requested type is a sub type of the assigned value" in {
            class A
            class B extends A
            val name = random[MetaInfo.Name]
            val value = new A
            val origin = fixture.create[AnyRef]()

            origin.meta(name) = value

            origin.meta[B](name) should not be('defined)
          }
        }
      }
    }

    def withType(fixture: OriginBehaviors.Fixture.WithNoRead) {
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
    }

    def withFilteredRead(fixture: OriginBehaviors.Fixture.WithFilteredRead) {
      "invokes the specified filter" in {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        val origin = fixture.create[AnyRef] {() => None}

        origin(filter)

        verify(filter).apply(anything())
      }

      "if it passes the specified filter" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        when(filter.apply(anything())) thenReturn true

        "use the specified read function" in {
          val read = mock[Origin.Read.Unfiltered[AnyRef]]("Origin.read")
          when(read.apply()) thenReturn None
          val origin = fixture.create(read)

          origin(filter)

          verify(read).apply()
        }

        "if reading is successful" should {
          "return a property" which {
            "has the same name as the origin" in {
              val origin = fixture.create {() => Some(random[String])}

              val property = origin(filter).value
              property.name should be(origin.name)
            }

            "contains the result of the specified read function" in {
              val result = random[String]
              val origin = fixture.create {() => Some(result)}

              val property = origin(filter).value
              property.value should be(result)
            }
          }
        }

        "if reading is unsuccessful" should {
          "return None" in {
            val origin = fixture.create[AnyRef] {() => None}

            origin(filter) should not be('defined)
          }
        }
      }

      "if it does not pass the specified filter" should {
        val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
        when(filter.apply(anything())) thenReturn false

        "not invoke the specified read function" in {
          val read = mock[Origin.Read.Unfiltered[AnyRef]]("Origin.read")
          val origin = fixture.create(read)

          origin(filter)

          verify(read, never()).apply()
        }

        "return None" in {
          val origin = fixture.create[AnyRef]()

          origin(filter) should not be('defined)
        }
      }
    }

    def withUnfilteredRead(fixture: OriginBehaviors.Fixture.WithUnfilteredRead) {
      val filter = mock[Filter[Origin.Meta.Readable]]("Filter")

      "uses the specified read function" in {
        val read = mock[Origin.Read.Filtered[AnyRef]]("Origin.read")
        when(read.apply(anything())) thenReturn None
        val origin = fixture.create(read)

        origin(filter)

        verify(read).apply(filter)
      }

      "if reading is successful" should {
        "return a property" which {
          "has the same name as the origin" in {
            val origin = fixture.create {_ => Some(random[String])}

            val property = origin(filter).value
            property.name should be(origin.name)
          }

          "contains the result of the specified read function" in {
            val result = random[String]
            val origin = fixture.create {_ => Some(result)}

            val property = origin(filter).value
            property.value should be(result)
          }
        }
      }

      "if reading is unsuccessful" should {
        "return None" in {
          val origin = fixture.create[AnyRef] {(_: Filter[Meta.Readable]) => None}

          origin(filter) should not be('defined)
        }
      }
    }

    def withRead(fixture: OriginBehaviors.Fixture) {
      "if it is created with a filtered read" which {
        behave like withFilteredRead(fixture)
      }

      "if it is created with an unfiltered read" which {
        behave like withUnfilteredRead(fixture)
      }
    }
  }
}

object OriginBehaviors {

  object Fixture {

    trait WithNoRead extends Randoms {

      protected type Origin[+A <: AnyRef] <: amber.Origin[A]

      def create[A <: AnyRef : NotNothing : Manifest]
        (name: Property.Name = random[Property.Name],
         family: Family = random[Family]): Origin[A]
    }

    trait WithFilteredRead extends WithNoRead {
      def create[A <: AnyRef : NotNothing : Manifest]
        (read: Origin.Read.Unfiltered[A]): Origin[A]
    }

    trait WithUnfilteredRead extends WithNoRead {
      def create[A <: AnyRef : NotNothing : Manifest]
        (read: Origin.Read.Filtered[A]): Origin[A]
    }
  }

  trait Fixture extends Fixture.WithFilteredRead with Fixture.WithUnfilteredRead {

    def create[A <: AnyRef : NotNothing : Manifest, B : Origin.Read[A]#apply]
      (name: Property.Name, family: Family, read: B): Origin[A]

    override def create[A <: AnyRef : NotNothing : Manifest]
        (name: Property.Name, family: Family) =
      create[A, Origin.Read.Unfiltered[A]](name, family, {() => None})

    override def create[A <: AnyRef : NotNothing : Manifest]
        (read: Origin.Read.Unfiltered[A]) =
      create(random[Property.Name], random[Family], read)

    override def create[A <: AnyRef : NotNothing : Manifest]
        (read: Origin.Read.Filtered[A]) =
      create(random[Property.Name], random[Family], read)
  }
}
