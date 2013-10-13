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

package amber

import scala.language.higherKinds

import scala.concurrent.Await

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.{verify, when}

import amber.util.Type

sealed trait OriginBehaviors {
  this: Spec =>

  type Origin[+A] <: amber.Origin[A]

  def copoint[A, B](reading: Origin[A]#Reading[B]): Option[B]
  def build[A: Type](name: Origin.Name, family: Origin.Family)(read: Fixture.Read[A]): Origin[A]

  trait Mapper {
    def apply[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B): Origin[B]
  }

  trait Fixture {

    val family = random[Origin.Family]
    val read = mock[Fixture.Read[A]]("Origin.read")
    val f = mock[A => B]("Origin.map")

    def create(): Origin[A] = build(random[Origin.Name], family)(read)
    def create(name: Origin.Name): Origin[A] = build(name, family)(read)
    def create(family: Origin.Family): Origin[A] = build(random[Origin.Name], family)(read)
    def create[A: Type](read: Fixture.Read[A]): Origin[A] = build(random[Origin.Name], family)(read)

    def map(underlying: Origin[A])(implicit mapper: Mapper) =
      mapper(underlying, random[Origin.Name])(f)

    def map(underlying: Origin[A], name: Origin.Name)(implicit mapper: Mapper) =
      mapper(underlying, name)(f)

    def map[B: Type](underlying: Origin[A], f: A => B)(implicit mapper: Mapper) =
      mapper(underlying, random[Origin.Name])(f)
  }

  object Fixture {
    type Read[+A] = () => Origin.Local.Reading[A]
  }

  class A
  class U extends A
  class B
  class V extends B

  def anOrigin() {
    "has the specified name" in {
      new Fixture {
        val name = random[Origin.Name]
        val origin = create(name)

        origin.name should be(name)
      }
    }

    "is in the specified family" in {
      new Fixture {
        val origin = create(family)

        origin.family should be(family)
      }
    }

    "if a meta value was never assigned" should {
      "return None " in {
        new Fixture {
          val origin = create()

          copoint(origin.selectDynamic(random[Origin.MetaInfo.Name])).as[Any] should not be('defined)
        }
      }
    }

    "if a meta value was previously assigned" should {
      "return the assigned value" in {
        new Fixture {
          val origin = create()

          val name = random[Origin.MetaInfo.Name]
          val value = random[Int]
          origin(name) = value

          copoint(origin.selectDynamic(name)).as[Int].value should be(value)
        }
      }
    }

    "does return the same type" in {
      new Fixture {
        val origin = create(mock[Fixture.Read[A]]("Origin.read"))

        origin.returns[A] should be(true)
      }
    }

    "does return a super type" in {
      new Fixture {
        val origin = create(mock[Fixture.Read[U]]("Origin.read"))

        origin.returns[A] should be(true)
      }
    }

    "does not return a different type" in {
      new Fixture {
        val origin = create(mock[Fixture.Read[A]]("Origin.read"))

        origin.returns[B] should be(false)
      }
    }

    "does not return a sub type" in {
      new Fixture {
        val origin = create(mock[Fixture.Read[A]]("Origin.read"))

        origin.returns[U] should be(false)
      }
    }

    "use the specified read function" in {
      new Fixture {
        when(read.apply()) thenReturn None
        val origin = create()

        copoint(origin.read())

        verify(read).apply()
      }
    }

    "if reading is successful" should {
      "return a value" which {
        "has the same name as the origin" in {
          new Fixture {
            when(read()) thenReturn Some(new A)
            val origin = create()

            val (Origin.Value(name, _), _) = copoint(origin.read()).value
            name should be(origin.name)
          }
        }

        "contains the result of the specified read function" in {
          new Fixture {
            val result = new A
            when(read()) thenReturn Some(result)
            val origin = create()

            val (Origin.Value(_, value), _) = copoint(origin.read()).value
            value should be(result)
          }
        }
      }

      "contains the origin's meta" in {
        new Fixture {
          when(read()) thenReturn Some(new A)
          val origin = create()

          val name = random[String]
          val value = random[Int]
          origin(name) = value

          val (_, meta) = copoint(origin.read()).value
          meta.selectDynamic(name).as[Any].value should be(value)
        }
      }
    }

    "if reading is unsuccessful" should {
      "return None" in {
        new Fixture {
          when(read()) thenReturn None
          val origin = create()

          copoint(origin.read()) should not be('defined)
        }
      }
    }

    "if mapped over" should {
      behave like aMappedOrigin(new Mapper {
        override def apply[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B) =
          underlying.map(name)(f).asInstanceOf[Origin[B]]
      })
    }
  }

  def aMappedOrigin(implicit mapper: Mapper) {
    "have the specified name" in {
      new Fixture {
        val name = random[Origin.Name]
        val underlying = create()
        val origin = map(underlying, name)

        origin.name should be(name)
      }
    }

    "be in same family" in {
      new Fixture {
        val underlying = create(family)
        val origin = map(underlying)

        origin.family should be(family)
      }
    }

    "return None" when {
      "a meta value was never assigned" in {
        new Fixture {
          val underlying = create()
          val origin = map(underlying)

          copoint(origin.selectDynamic(random[Origin.MetaInfo.Name])).as[Any] should not be('defined)
        }
      }
    }

    "return the assigned value" when {
      "a meta value was previously assigned to the mapped origin" in {
        new Fixture {
          val underlying = create()
          val origin = map(underlying)

          val name = random[Origin.MetaInfo.Name]
          val value = random[Int]
          underlying(name) = random[Int]
          origin(name) = value

          copoint(origin.selectDynamic(name)).as[Int].value should be(value)
        }
      }

      "a meta value was previously assigned to the underlying origin" in {
        new Fixture {
          val underlying = create()
          val origin = map(underlying)

          val name = random[Origin.MetaInfo.Name]
          val value = random[Int]
          underlying(name) = value

          copoint(origin.selectDynamic(name)).as[Int].value should be(value)
        }
      }
    }

    "return the same type" in {
      new Fixture {
        val underlying = create()
        val origin = map(underlying, mock[A => B]("Origin.map"))

        origin.returns[B] should be(true)
      }
    }

    "return a super type" in {
      new Fixture {
        val underlying = create()
        val origin = map(underlying, mock[A => U]("Origin.map"))

        origin.returns[A] should be(true)
      }
    }

    "not return a different type" in {
      new Fixture {
        val underlying = create()
        val origin = map(underlying, mock[A => B]("Origin.map"))

        origin.returns[A] should be(false)
      }
    }

    "not return a sub type" in {
      new Fixture {
        val underlying = create()
        val origin = map(underlying, mock[A => B]("Origin.map"))

        origin.returns[V] should be(false)
      }
    }

    "use the underlying origin's read function" in {
      new Fixture {
        when(read()) thenReturn None
        val underlying = create()
        val origin = map(underlying)

        origin.read()

        verify(read).apply()
      }
    }

    "invoke the specified map function with result of the underlying origin's read function" in {
      new Fixture {
        val result = new A
        when(read()) thenReturn Some(result)
        val underlying = create()
        val origin = map(underlying)

        origin.read()

        verify(f).apply(result)
      }
    }

    "return a result" when {
      "reading is successful" which {
        "has the same name as the mapped origin" in {
          new Fixture {
            when(read()) thenReturn Some(new A)
            val underlying = create()
            when(f(anything())) thenReturn new B
            val origin = map(underlying)

            val (Origin.Value(name, _), _) = copoint(origin.read()).value
            name should be(origin.name)
          }
        }

        "contains the result of the specified map function" in {
          new Fixture {
            when(read()) thenReturn Some(new A)
            val underlying = create()

            val result = new B
            when(f(anything())) thenReturn result
            val origin = map(underlying)

            val (Origin.Value(_, value), _) = copoint(origin.read()).value
            value should be(result)
          }
        }

        "contains the mapped origin's meta" in {
          new Fixture {
            when(read()) thenReturn Some(new A)
            val underlying = create()
            when(f(anything())) thenReturn new B
            val origin = map(underlying)

            val name = random[String]
            val value = random[Int]
            underlying(name) = random[Int]
            origin(name) = value

            val (_, meta) = copoint(origin.read()).value
            meta.selectDynamic(name).as[Any].value should be(value)
          }
        }

        "contains the underlying origin's meta" in {
          new Fixture {
            when(read()) thenReturn Some(new A)
            val underlying = create()
            when(f(anything())) thenReturn new B
            val origin = map(underlying)

            val name = random[String]
            val value = random[Int]
            underlying(name) = value

            val (_, meta) = copoint(origin.read()).value
            meta.selectDynamic(name).as[Any].value should be(value)
          }
        }
      }
    }

    "return None" when {
      "reading is unsuccessful" in {
        new Fixture {
          when(read()) thenReturn None
          val underlying = create()
          val origin = map(underlying)

          copoint(origin.read()) should not be('defined)
        }
      }
    }
  }
}

object OriginBehaviors {

  trait Local extends OriginBehaviors {
    this: Spec =>

    override type Origin[+A] <: Origin.Local[A]

    override def copoint[A, B](reading: Origin[A]#Reading[B]) = reading
  }

  trait Remote extends OriginBehaviors {
    this: Spec =>

    override type Origin[+A] <: Origin.Remote[A]

    override def copoint[A, B](reading: Origin[A]#Reading[B]) = Await.result(reading.run, timeout)
  }
}
