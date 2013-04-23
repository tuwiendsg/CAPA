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

import scala.concurrent.{Await, Future}

import scalaz.Id.Id
import scalaz.OptionT

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{never, verify, when}

import util.Type

sealed trait ProcessingBehaviors[X[+_]] extends mock.origin.BuilderComponent
                                        with mock.origin.BuilderComponent.InSpec {
  this: Spec with Processing[X] =>

  def point[A, B](origin: Origin[A])(value: Option[B]): origin.Reading[B]
  def copoint[A, B](reading: Origin[A]#Reading[B]): Option[B]

  def build[A: Type](name: Origin.Name)(read: Fixture.Read[A]): Origin[A]

  abstract override def mocker[A](implicit typeA: Type[A]) =
    super.mocker[A] andThen {case ((name, family, read), origin) =>
      when(origin.name) thenReturn name
      when(origin.family) thenReturn family
      when(origin.returns(anything(), equalTo(typeA))) thenReturn true

      val meta = mock[Origin.MetaInfo]("Origin.MetaInfo")
      when(meta.selectDynamic(anything())) thenAnswer {
        args: Array[AnyRef] =>
          copoint(origin.selectDynamic(args(0).asInstanceOf[Origin.MetaInfo.Name]))
      }
      when(origin.read()) thenAnswer {_: Array[AnyRef] => read(meta)}
      when(origin.map(anything(): Origin.Name)(anything())(anything())) thenAnswer {
        args: Array[AnyRef] =>
          val name = args(0).asInstanceOf[Origin.Name]
          val f = args(1).asInstanceOf[A => Any]
          val typeB = args(2).asInstanceOf[Type[Any]]

          builder.map(origin, name)(f)(typeB)
      }
    }

  class A
  class B
  class C

  trait Fixture {

    val input = random[Origin.Name]
    val output = random[Origin.Name]
    val processor = mock[A => B]("Processor")
    val definition = mock[PartialFunction[Origin.Name, (Origin.Name, A => B)]]("Process.Definition")
    val read = mock[Fixture.Read[A]]("Origin.read")

    when(definition isDefinedAt input) thenReturn true
    when(definition apply input) thenReturn (output -> processor)

    def create(): Origin[A] = build(input)(read)
  }

  object Fixture {
    type Read[+A] = () => Origin.Local.Reading[A]
  }

  def aProcessing {
    "invoke the process definition" when {
      "an origin was created before" in {
        new Fixture {
          create()
          process(definition)

          verify(definition) isDefinedAt input
          verify(definition) apply input
        }
      }

      "an origin is created with matching name and type" in {
        new Fixture {
          process(definition)
          create()

          verify(definition) isDefinedAt input
          verify(definition) apply input
        }
      }
    }

    "not invoke the process definition" when {
      "an origin is created with non-matching name" in {
        new Fixture {
          when(definition isDefinedAt input) thenReturn false

          process(definition)
          build(different(input))(read)

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }

      "an origin is created with non-matching type" in {
        new Fixture {
          process(definition)
          build(input)(mock[Fixture.Read[C]]("Origin.read"))

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }
    }

    "create an additional origin" which {
      "has expected name" in {
        new Fixture {
          process(definition)
          create()

          built.last.name should be(output)
        }
      }

      "has same family as the underlying origin" in {
        new Fixture {
          process(definition)
          val underlying = create()

          built.last.family should be(underlying.family)
        }
      }

      "has expected type" in {
        new Fixture {
          process(definition)
          create()

          built.last.returns[B] should be(true)
        }
      }

      "when read" should {
        "read the underlying origin" in {
          new Fixture {
            process(definition)
            val underlying = create()
            when(read()) thenReturn None

            built.last.read()

            verify(underlying).read()
          }
        }

        "use the result of the underlying origin as input for the processor" in {
          new Fixture {
            val value = new A
            when(read()) thenReturn Some(value)

            process(definition)
            create()

            built.last.read()

            verify(processor).apply(value)
          }
        }

        "return the result of the processor" in {
          new Fixture {
            val value = new B
            when(processor(anything())) thenReturn value
            when(read()) thenReturn Some(new A)

            process(definition)
            create()

            val (result, _) = copoint(built.last.read()).value

            result.name should be(output)
            result.value should be(value)
          }
        }

        "if processor doesn't define a meta value, return the underlying origin's meta value" in {
          new Fixture {
            process(definition)
            val underlying = create()
            when(processor(anything())) thenReturn new B
            when(read()) thenReturn Some(new A)

            val name = random[String]
            val value = new B
            val origin = built.last.asInstanceOf[Origin[Any]]
            when(origin.selectDynamic(name)) thenReturn point(origin)(None)
            when(underlying.selectDynamic(name)) thenReturn point(underlying)(Some(new Origin.MetaInfo.Value(value)))

            val (_, meta) = copoint(origin.read()).value

            meta.selectDynamic(name).as[Any].value should be(value)
          }
        }

        "if processor does define a meta value, return that meta value" in {
          new Fixture {
            process(definition)
            val underlying = create()
            when(processor(anything())) thenReturn new B
            when(read()) thenReturn Some(new A)

            val name = random[String]
            val value = new B
            val origin = built.last.asInstanceOf[Origin[Any]]
            when(origin.selectDynamic(name)) thenReturn point(origin)(Some(new Origin.MetaInfo.Value(value)))

            val (_, meta) = copoint(origin.read()).value

            meta.selectDynamic(name).as[Any].value should be(value)
          }
        }

        "return None if the result of the underlying origin is not defined" in {
          new Fixture {
            when(read()) thenReturn None

            process(definition)
            create()

            copoint(built.last.read()) should not be('defined)
            verify(processor, never()).apply(anything())
          }
        }
      }
    }
  }
}

object ProcessingBehaviors {

  trait Local extends ProcessingBehaviors[Id] with mock.origin.BuilderComponent.Local {
    this: Spec with Processing.Local =>

    override def point[A, B](origin: Origin[A])(value: Option[B]) = value
    override def copoint[A, B](reading: Origin[A]#Reading[B]) = reading

    override def build[A: Type](name: Origin.Name)(read: Fixture.Read[A]) =
      builder.build(name, random[Origin.Family]) {meta => read() map {
        value => (Origin.Value(name, value), meta)
      }}
  }

  trait Remote extends ProcessingBehaviors[Future] with mock.origin.BuilderComponent.Remote {
    this: Spec with Processing.Remote =>

    override def point[A, B](origin: Origin[A])(value: Option[B]) = OptionT(Future.successful(value))
    override def copoint[A, B](reading: Origin[A]#Reading[B]) = Await.result(reading.run, timeout)

    override def build[A: Type](name: Origin.Name)(read: Fixture.Read[A]) =
      builder.build(name, random[Origin.Family]) {meta => OptionT(Future.successful(read() map {
        value => (Origin.Value(name, value), meta)
      }))}
  }
}
