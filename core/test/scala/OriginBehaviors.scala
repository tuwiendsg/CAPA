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

import scala.language.higherKinds

import scala.concurrent.{Await, Future}

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{never, verify, when}

import util.{Filter, NotNothing, Type}

sealed trait OriginBehaviors {
  this: Spec =>

  type Origin[+A] <: amber.Origin[A]

  def fixture: Fixture

  trait Fixture {

    def read[A](origin: Origin[A]): Option[(Origin.Value[A], Origin.MetaInfo)]
    def getMeta[A](origin: Origin[A], name: Origin.MetaInfo.Name): Option[Origin.MetaInfo.Value[_]]
    def create[A: Type](name: Origin.Name, family: Origin.Family, read: Fixture.Read[A]): Origin[A]

    def create[A: NotNothing : Type](): Origin[A] = create(random[Origin.Name])

    def create[A: NotNothing : Type](name: Origin.Name): Origin[A] =
      create(name, random[Origin.Family])

    def create[A: NotNothing : Type](family: Origin.Family): Origin[A] =
      create(random[Origin.Name], family)

    def create[A: NotNothing : Type](name: Origin.Name, family: Origin.Family): Origin[A] =
      create(name, family, mock[Fixture.Read[A]]("Origin.read"))

    def create[A: Type](read: Fixture.Read[A]): Origin[A] =
      create(random[Origin.Name], random[Origin.Family], read)
  }

  object Fixture {
    type Read[+A] = () => Origin.Local.Reading[A]
  }

  class A
  class U extends A
  class B

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

        fixture.getMeta(origin, random[Origin.MetaInfo.Name]).as[Any] should not be('defined)
      }
    }

    "if a meta value was previously assigned" should {
      "return the assigned value" in {
        val origin = fixture.create[Any]()

        val name = random[Origin.MetaInfo.Name]
        val value = random[Int]
        origin(name) = value

        fixture.getMeta(origin, name).as[Int].value should be(value)
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

          val (Origin.Value(name, _), _) = fixture.read(origin).value
          name should be(origin.name)
        }

        "contains the result of the specified read function" in {
          val result = random[String]
          val read = mock[Fixture.Read[String]]("Origin.read")
          when(read()) thenReturn Some(result)
          val origin = fixture.create(read)

          val (Origin.Value(_, value), _) = fixture.read(origin).value
          value should be(result)
        }
      }

      "contains the origin's meta" in {
        val read = mock[Fixture.Read[String]]("Origin.read")
        when(read()) thenReturn Some(random[String])
        val origin = fixture.create(read)

        val name = random[String]
        val value = random[Int]
        origin(name) = value

        val (_, meta) = fixture.read(origin).value
        meta.selectDynamic(name).as[Any].value should be(value)
      }
    }

    "if reading is unsuccessful" should {
      "return None" in {
        val read = mock[Fixture.Read[_]]("Origin.read")
        when(read()) thenReturn None
        val origin = fixture.create(read)

        fixture.read(origin) should not be('defined)
      }
    }

    "if mapped over" should {
      behave like aMappedOrigin(new Mapper {
        override def apply[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B) =
          underlying.map(name)(f).asInstanceOf[Origin[B]]
      })
    }
  }

  trait Mapper {
    def apply[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B): Origin[B]
  }

  def aMappedOrigin(mapper: Mapper) {
    "have the specified name" in {
        val underlying = fixture.create[Any]()
        val name = random[Origin.Name]
        val origin = mapper(underlying, name)(identity)

        origin.name should be(name)
      }

      "be in same family" in {
        val family = random[Origin.Family]
        val underlying = fixture.create[Any](family)
        val origin = mapper(underlying, random[Origin.Name])(identity)

        origin.family should be(family)
      }

      "return None" when {
        "a meta value was never assigned" in {
          val underlying = fixture.create[Any]()
          val origin = mapper(underlying, random[Origin.Name])(identity)

          fixture.getMeta(origin, random[Origin.MetaInfo.Name]).as[Any] should not be('defined)
        }
      }

      "return the assigned value" when {
        "a meta value was previously assigned to the mapped origin" in {
          val underlying = fixture.create[Any]()
          val origin = mapper(underlying, random[Origin.Name])(identity)

          val name = random[Origin.MetaInfo.Name]
          val value = random[Int]
          underlying(name) = random[Int]
          origin(name) = value

          fixture.getMeta(origin, name).as[Int].value should be(value)
        }

        "a meta value was previously assigned to the underlying origin" in {
          val underlying = fixture.create[Any]()
          val origin = mapper(underlying, random[Origin.Name])(identity)

          val name = random[Origin.MetaInfo.Name]
          val value = random[Int]
          underlying(name) = value

          fixture.getMeta(origin, name).as[Int].value should be(value)
        }
      }

      "return the same type" in {
        val underlying = fixture.create[B]()
        val origin = mapper(underlying, random[Origin.Name])(mock[B => A]("Origin.map"))

        origin.returns[A] should be(true)
      }

      "return a super type" in {
        val underlying = fixture.create[B]()
        val origin = mapper(underlying, random[Origin.Name])(mock[B => U]("Origin.map"))

        origin.returns[A] should be(true)
      }

      "not return a different type" in {
        val underlying = fixture.create[B]()
        val origin = mapper(underlying, random[Origin.Name])(mock[B => A]("Origin.map"))

        origin.returns[B] should be(false)
      }

      "not return a sub type" in {
        val underlying = fixture.create[B]()
        val origin = mapper(underlying, random[Origin.Name])(mock[B => A]("Origin.map"))

        origin.returns[U] should be(false)
      }

      "use the underlying origin's read function" in {
        val read = mock[Fixture.Read[B]]("Origin.read")
        when(read.apply()) thenReturn None
        val underlying = fixture.create(read)
        val origin = mapper(underlying, random[Origin.Name])(mock[B => A]("Origin.map"))

        origin.read()

        verify(read).apply()
      }

      "invoke the specified map function with result of the underlying origin's read function" in {
        val result = random[String]
        val read = mock[Fixture.Read[String]]("Origin.read")
        when(read.apply()) thenReturn Some(result)
        val underlying = fixture.create(read)
        val map = mock[String => A]("Origin.map")
        val origin = mapper(underlying, random[Origin.Name])(map)

        origin.read()

        verify(map).apply(result)
      }

      "return a result" when {
        "reading is successful" which {
          "has the same name as the mapped origin" in {
            val read = mock[Fixture.Read[String]]("Origin.read")
            when(read()) thenReturn Some(random[String])
            val underlying = fixture.create(read)
            val origin = mapper(underlying, random[Origin.Name])(identity)

            val (Origin.Value(name, _), _) = fixture.read(origin).value
            name should be(origin.name)
          }

          "contains the result of the specified map function" in {
            val read = mock[Fixture.Read[String]]("Origin.read")
            when(read()) thenReturn Some(random[String])
            val underlying = fixture.create(read)

            val result = random[Int]
            val map = mock[String => Int]("Origin.map")
            when(map(anything())) thenReturn result
            val origin = mapper(underlying, random[Origin.Name])(map)

            val (Origin.Value(_, value), _) = fixture.read(origin).value
            value should be(result)
          }

          "contains the mapped origin's meta" in {
            val read = mock[Fixture.Read[String]]("Origin.read")
            when(read()) thenReturn Some(random[String])
            val underlying = fixture.create(read)
            val origin = mapper(underlying, random[Origin.Name])(identity)

            val name = random[String]
            val value = random[Int]
            underlying(name) = random[Int]
            origin(name) = value

            val (_, meta) = fixture.read(origin).value
            meta.selectDynamic(name).as[Any].value should be(value)
          }

          "contains the underlying origin's meta" in {
            val read = mock[Fixture.Read[String]]("Origin.read")
            when(read()) thenReturn Some(random[String])
            val underlying = fixture.create(read)
            val origin = mapper(underlying, random[Origin.Name])(identity)

            val name = random[String]
            val value = random[Int]
            underlying(name) = value

            val (_, meta) = fixture.read(origin).value
            meta.selectDynamic(name).as[Any].value should be(value)
          }
        }
      }

      "return None" when {
        "reading is unsuccessful" in {
          val read = mock[Fixture.Read[A]]("Origin.read")
          when(read()) thenReturn None
          val underlying = fixture.create(read)
          val origin = mapper(underlying, random[Origin.Name])(identity)

          fixture.read(origin) should not be('defined)
        }
      }
  }
}

object OriginBehaviors {

  trait Local extends OriginBehaviors {
    this: Spec =>

    override type Origin[+A] <: Origin.Local[A]

    override def fixture: Fixture
    trait Fixture extends super.Fixture {
      override def read[A](origin: Origin[A]) = origin.read()
      override def getMeta[A](origin: Origin[A], name: Origin.MetaInfo.Name) =
        origin.selectDynamic(name)
    }
  }

  trait Remote extends OriginBehaviors {
    this: Spec =>

    override type Origin[+A] <: Origin.Remote[A]

    override def fixture: Fixture
    trait Fixture extends super.Fixture {
      override def read[A](origin: Origin[A]) = Await.result(origin.read().run, timeout)
      override def getMeta[A](origin: Origin[A], name: Origin.MetaInfo.Name) =
        Await.result(origin.selectDynamic(name).run, timeout)
    }
  }
}
