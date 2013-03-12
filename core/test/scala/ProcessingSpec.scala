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

import scala.collection.immutable.HashMap

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{never, verify, when}

import util.{Filter, Type}

class ProcessingSpec extends Spec
                     with mock.origin.BuilderComponent.Local.Default
                     with mock.origin.BuilderComponent.InSpec
                     with origin.FinderComponent.Local.Default
                     with family.FinderComponent.Default
                     with origin.FactoryComponent.Default
                     with family.MemberFactoryComponent.Default
                     with Processing {

  override def mocker[A](implicit typeA: Type[A]) =
    super.mocker[A] andThen {case ((name, family, read), origin) =>
      when(origin.name) thenReturn name
      when(origin.family) thenReturn family
      when(origin.returns(anything(), equalTo(typeA))) thenReturn true

      val meta = mock[Origin.MetaInfo]("Origin.MetaInfo")
      when(meta.selectDynamic(anything())) thenAnswer {
        args: Array[AnyRef] =>
          origin.selectDynamic(args(0).asInstanceOf[Origin.MetaInfo.Name])
      }
      when(origin.read()) thenAnswer {_: Array[AnyRef] => read(meta)}
    }

  class A
  class B
  class C

  trait Fixture {

    val input = random[Origin.Name]
    val output = random[Origin.Name]
    val processor = mock[A => B]("Processor")
    val definition = mock[PartialFunction[Origin.Name, (Origin.Name, A => B)]]("Process.Definition")
    val read = mock[OriginFactory.Read[A]]("Origin.read")

    when(definition isDefinedAt input) thenReturn true
    when(definition apply input) thenReturn (output -> processor)
  }

  "Processing.process" when {
    "an origin was created before" should {
      "invoke the process definition with the origin" in {
        new Fixture {
          val name = random[Origin.Name]
          origin.create(name)(read)
          process(definition)

          verify(definition) isDefinedAt name
        }
      }
    }

    "an origin is created with matching name and type" should {
      "invoke the process definition" in {
        new Fixture {
          process(definition)
          origin.create(input)(read)

          verify(definition) isDefinedAt input
          verify(definition) apply input
        }
      }

      "create an additional origin" which {
        "has expected name" in {
          new Fixture {
            process(definition)
            origin.create(input)(read)

            built.last.name should be(output)
          }
        }

        "has same family as the underlying origin" in {
          new Fixture {
            process(definition)
            val underlying = origin.create(input)(read)

            built.last.family should be(underlying.family)
          }
        }

        "has expected type" in {
          new Fixture {
            process(definition)
            origin.create(input)(read)

            built.last.returns[B] should be(true)
          }
        }

        "when read" should {
          "read the underlying origin" in {
            new Fixture {
              process(definition)
              val underlying = origin.create(input)(read)
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
              origin.create(input)(read)

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
              origin.create(input)(read)

              val (result, _) = built.last.read().value

              result.name should be(output)
              result.value should be(value)
            }
          }

          "if processor doesn't define a meta value, return the underlying origin's meta value" in {
            new Fixture {
              process(definition)
              val underlying = origin.create(input)(read)
              when(processor(anything())) thenReturn new B
              when(read()) thenReturn Some(new A)

              val name = random[String]
              val value = new B
              when(built.last.selectDynamic(name)) thenReturn None
              when(underlying.selectDynamic(name)) thenReturn Some(new Origin.MetaInfo.Value(value))

              val (_, meta) = built.last.read().value

              meta.selectDynamic(name).as[Any].value should be(value)
            }
          }

          "if processor does define a meta value, return that meta value" in {
            new Fixture {
              process(definition)
              val underlying = origin.create(input)(read)
              when(processor(anything())) thenReturn new B
              when(read()) thenReturn Some(new A)

              val name = random[String]
              val value = new B
              when(built.last.selectDynamic(name)) thenReturn Some(new Origin.MetaInfo.Value(value))

              val (_, meta) = built.last.read().value

              meta.selectDynamic(name).as[Any].value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              process(definition)
              origin.create(input)(read)

              built.last.read() should not be('defined)
              verify(processor, never()).apply(anything())
            }
          }
        }
      }
    }

    "an origin is created with non-matching name" should {
      "not invoke the process definition" in {
        new Fixture {
          when(definition isDefinedAt input) thenReturn false

          process(definition)
          origin.create(different(input))(read)

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }
    }

    "an origin is created with non-matching type" should {
      "not invoke the process definition" in {
        new Fixture {
          process(definition)
          origin.create(input)(mock[OriginFactory.Read[C]]("Origin.read"))

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }
    }
  }
}
