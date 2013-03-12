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

import scala.concurrent.Future

import scalaz.Comonad
import scalaz.Id.{id, Id}

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{never, verify, when}

import util.{Filter, FutureComonad, NotNothing, Type}

sealed trait OriginBehaviors[X[+_]] {
  this: Spec =>

  type Origin[+A] <: amber.Origin[X, A]
  def X: Comonad[X]

  def fixture: Fixture

  trait Fixture {

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
    type Read[+A] = () => Option[A]
  }

  object anOrigin {

    class A
    class U extends A
    class B

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

        X.copoint(origin.selectDynamic(random[Origin.MetaInfo.Name]).run).as[Any] should not be('defined)
      }
    }

    "if a meta value was previously assigned" should {
      "return the assigned value" in {
        val name = random[Origin.MetaInfo.Name]
        val value = new A
        val origin = fixture.create[Any]()

        origin(name) = value

        X.copoint(origin.selectDynamic(name).run).as[A].value should be(value)
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

          val (Origin.Value(name, _), _) = X.copoint(origin.read().run).value
          name should be(origin.name)
        }

        "contains the result of the specified read function" in {
          val result = random[String]
          val read = mock[Fixture.Read[String]]("Origin.read")
          when(read()) thenReturn Some(result)
          val origin = fixture.create(read)

          val (Origin.Value(_, value), _) = X.copoint(origin.read().run).value
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

        val (_, meta) = X.copoint(origin.read().run).value
        meta.selectDynamic(name).as[Any].value should be(value)
      }
    }

    "if reading is unsuccessful" should {
      "return None" in {
        val read = mock[Fixture.Read[_]]("Origin.read")
        when(read()) thenReturn None
        val origin = fixture.create(read)

        X.copoint(origin.read().run) should not be('defined)
      }
    }
  }
}

object OriginBehaviors {

  trait Local extends OriginBehaviors[Id] {
    this: Spec =>

    override type Origin[+A] <: Origin.Local[A]

    override val X = id
  }

  trait Remote extends OriginBehaviors[Future] with FutureComonad {
    this: Spec =>

    override type Origin[+A] <: Origin.Remote[A]

    override val X = implicitly[Comonad[Future]]
  }
}
