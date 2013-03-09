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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{never, verify, when}

import util.{Filter, NotNothing}

trait OriginBehaviors {
  this: Spec =>

  class A
  class U extends A
  class B

  def fixture: Fixture

  trait Fixture {

    def create[A: ClassTag : TypeTag](name: Origin.Name,
                                      family: Origin.Family,
                                      read: Fixture.Read[A]): Origin[A]

    def create[A: NotNothing : ClassTag: TypeTag](): Origin[A] = create(random[Origin.Name])

    def create[A: NotNothing : ClassTag: TypeTag](name: Origin.Name): Origin[A] =
      create(name, random[Origin.Family])

    def create[A: NotNothing : ClassTag: TypeTag](family: Origin.Family): Origin[A] =
      create(random[Origin.Name], family)

    def create[A: NotNothing : ClassTag : TypeTag](name: Origin.Name,
                                                   family: Origin.Family): Origin[A] =
      create(name, family, mock[Fixture.Read[A]]("Origin.read"))

    def create[A: ClassTag : TypeTag](read: Fixture.Read[A]): Origin[A] =
      create(random[Origin.Name], random[Origin.Family], read)
  }

  object Fixture {
    type Read[+A] = () => Option[A]
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

        origin.selectDynamic(random[Origin.MetaInfo.Name]).as[Any] should not be('defined)
      }
    }

    "if a meta value was previously assigned" should {
      "return the assigned value" when {
        "requested type is same type as the assigned value" in {
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin(name) = value

          origin.selectDynamic(name).as[A].value should be(value)
        }

        "requested type is a super type of the assigned value" in {
          val name = random[Origin.MetaInfo.Name]
          val value = new U
          val origin = fixture.create[Any]()

          origin(name) = value

          origin.selectDynamic(name).as[A].value should be(value)
        }
      }

      "return None " when {
        "requested type is a different type than the assigned value" in {
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin(name) = value

          origin.selectDynamic(name).as[B] should not be('defined)
        }

        "requested type is a sub type of the assigned value" in {
          val name = random[Origin.MetaInfo.Name]
          val value = new A
          val origin = fixture.create[Any]()

          origin(name) = value

          origin.selectDynamic(name).as[U] should not be('defined)
        }
      }
    }

    "does return the same type" in {
      val origin = fixture.create[A]()

      origin.returns[A] should be(true)
    }

    "does return a super type" in {
      val origin = fixture.create[U]()

      origin.returns[A] should be(true)
    }

    "does not return a different type" in {
      val origin = fixture.create[A]()

      origin.returns[B] should be(false)
    }

    "does not return a sub type" in {
      val origin = fixture.create[A]()

      origin.returns[U] should be(false)
    }

    "use the specified read function" in {
      val read = mock[Fixture.Read[_]]("Origin.read")
      when(read.apply()) thenReturn None
      val origin = fixture.create(read)

      origin.read()

      verify(read).apply()
    }

    "if reading is successful" should {
      "return a value" which {
        "has the same name as the origin" in {
          val read = mock[Fixture.Read[String]]("Origin.read")
          when(read()) thenReturn Some(random[String])
          val origin = fixture.create(read)

          val (Origin.Value(name, _), _) = origin.read().value
          name should be(origin.name)
        }

        "contains the result of the specified read function" in {
          val result = random[String]
          val read = mock[Fixture.Read[String]]("Origin.read")
          when(read()) thenReturn Some(result)
          val origin = fixture.create(read)

          val (Origin.Value(_, value), _) = origin.read().value
          value should be(result)
        }
      }

      "return origin's meta" in {
        val read = mock[Fixture.Read[String]]("Origin.read")
        when(read()) thenReturn Some(random[String])
        val origin = fixture.create(read)

        val name = random[String]
        val value = new A
        origin(name) = value

        val (_, meta) = origin.read().value
        meta.selectDynamic(name).as[Any].value should be(value)
      }
    }

    "if reading is unsuccessful" should {
      "return None" in {
        val read = mock[Fixture.Read[_]]("Origin.read")
        when(read()) thenReturn None
        val origin = fixture.create(read)

        origin.read() should not be('defined)
      }
    }
  }
}
