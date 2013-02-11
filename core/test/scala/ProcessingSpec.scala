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

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{never, verify, when}

import util.Filter

trait ProcessingSpec extends Spec
                     with mock.origin.BuilderComponent
                     with origin.FinderComponent.Default
                     with family.FinderComponent.Default
                     with origin.FactoryComponent.Default
                     with family.MemberFactoryComponent.Default
                     with Processing {

  override def mocker[A: ClassTag : TypeTag, B: Origin.Read[A]#apply] =
    super.mocker[A, B] andThen {case ((name, family, read, tag), origin) =>
      when(origin.name) thenReturn name
      when(origin.family) thenReturn family
      when(origin.returns(anything(), equalTo(tag))) thenReturn true
      when(origin.read(anything())) thenAnswer {
        args: Array[AnyRef] =>
          (read match {
            case f: Origin.Read.Unfiltered[_] => f()
            case f: Origin.Read.Filtered[_] => f(args(0).asInstanceOf[Filter[Origin.Meta.Readable]])
          }) map {Origin.Value(name, _)}
      }
    }
}

class ProcessSpec extends ProcessingSpec {

  class A
  class B
  class C

  trait Fixture {

    val input = random[Origin.Name]
    val output = random[Origin.Name]
    val processor = mock[A => B]("Processor")
    val definition = mock[PartialFunction[Origin.Name, (Origin.Name, A => B)]]("Process.Definition")
    val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")

    when(definition isDefinedAt input) thenReturn true
    when(definition apply input) thenReturn (output -> processor)
  }

  "Processing.process" when {
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
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "read the underlying origin" in {
            new Fixture {
              process(definition)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              built.last.read(filter)

              verify(underlying).read(filter)
            }
          }

          "use the result of the underlying's origin as input for the processor" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              process(definition)
              origin.create(input)(read)

              built.last.read(filter)

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

              val result = built.last.read(filter).value.asInstanceOf[Origin.Value[B]]

              result.name should be(output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              process(definition)
              origin.create(input)(read)

              built.last.read(filter) should not be('defined)
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
          origin.create(input)(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }
    }
  }
}

class MapSpec extends ProcessingSpec {

  class A
  class B
  class C

  trait Fixture {
    val input = random[Origin.Name]
    val output = random[Origin.Name]
    val mapper = mock[A => B]("Mapper")
    val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
  }

  "Processing.map" when {
    "an underlying origin is created with matching name and type" should {
      "create an additional origin" which {
        "has expected name" in {
          new Fixture {
            map(input, output)(mapper)
            origin.create(input)(read)

            built.last.name should be(output)
          }
        }

        "has same family as the underlying origin" in {
          new Fixture {
            map(input, output)(mapper)
            val underlying = origin.create(input)(read)

            built.last.family should be(underlying.family)
          }
        }

        "has expected type" in {
          new Fixture {
            map(input, output)(mapper)
            origin.create(input)(read)

            built.last.returns[B] should be(true)
          }
        }

        "when read" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "read the underlying origin" in {
            new Fixture {
              map(input, output)(mapper)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              built.last.read(filter)

              verify(underlying).read(filter)
            }
          }

          "use the result of the underlying's origin as input for the mapper" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              map(input, output)(mapper)
              origin.create(input)(read)

              built.last.read(filter)

              verify(mapper).apply(value)
            }
          }

          "return the result of the mapper" in {
            new Fixture {
              val value = new B
              when(mapper(anything())) thenReturn value
              when(read()) thenReturn Some(new A)

              map(input, output)(mapper)
              origin.create(input)(read)

              val result = built.last.read(filter).value.asInstanceOf[Origin.Value[B]]

              result.name should be(output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              map(input, output)(mapper)
              origin.create(input)(read)

              built.last.read(filter) should not be('defined)
              verify(mapper, never()).apply(anything())
            }
          }
        }
      }
    }

    "an origin is created with non-matching name" should {
      "not create an additional origin" in {
        new Fixture {
          map(input, output)(mapper)
          origin.create(different(input))(read)

          verify(build, never()).apply(equalTo(output), anything(), anything())
        }
      }
    }

    "an origin is created with non-matching type" should {
      "not create an additional origin" in {
        new Fixture {
          map(input, output)(mapper)
          origin.create(different(input))(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(build, never()).apply(equalTo(output), anything(), anything())
        }
      }
    }
  }
}

class OperationSpec extends ProcessingSpec {

  class A
  class B
  class C

  trait Fixture {
    val input = random[Origin.Name]
    val output = random[Operation.Name]
    val function = mock[A => B]("Mapper")
    val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
  }

  "Processing.operation" when {
    "an underlying origin is created with matching type" should {
      "create an additional origin" which {
        "has expected name" in {
          new Fixture {
            operation(output)(function)
            origin.create(input)(read)

            built.last.name should be(input / output)
          }
        }

        "has same family as the underlying origin" in {
          new Fixture {
            operation(output)(function)
            val underlying = origin.create(input)(read)

            built.last.family should be(underlying.family)
          }
        }

        "has expected type" in {
          new Fixture {
            operation(output)(function)
            origin.create(input)(read)

            built.last.returns[B] should be(true)
          }
        }

        "when read" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "read the underlying origin" in {
            new Fixture {
              operation(output)(function)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              built.last.read(filter)

              verify(underlying).read(filter)
            }
          }

          "use the result of the underlying's origin as input for the mapper" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              operation(output)(function)
              origin.create(input)(read)

              built.last.read(filter)

              verify(function).apply(value)
            }
          }

          "return the result of the mapper" in {
            new Fixture {
              val value = new B
              when(function(anything())) thenReturn value
              when(read()) thenReturn Some(new A)

              operation(output)(function)
              origin.create(input)(read)

              val result = built.last.read(filter).value.asInstanceOf[Origin.Value[B]]

              result.name should be(input / output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              operation(output)(function)
              origin.create(input)(read)

              built.last.read(filter) should not be('defined)
              verify(function, never()).apply(anything())
            }
          }
        }
      }
    }

    "an origin is created with non-matching type" should {
      "not create an additional origin" in {
        new Fixture {
          operation(output)(function)
          origin.create(different(input))(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(build, never()).apply(equalTo(input / output), anything(), anything())
        }
      }
    }
  }
}
