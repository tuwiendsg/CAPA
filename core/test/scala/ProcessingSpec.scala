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

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.{never, verify, when}

import util.Filter
import util.Events.observe

class ProcessingSpec extends Spec
                     with mock.origin.FactoryComponent
                     with mock.family.MemberFactoryComponent
                     with Processing {

  override protected type Origin[+A <: AnyRef] = amber.Origin[A]

  override def beforeEach() {
    super.beforeEach()

    observe(origin.created) {
      case (origin, _) => FamilyFinder.add(origin)
    }
  }

  "Process" when {

    trait Fixture {

      class A
      class B

      val input = random[Property.Name]
      val output = random[Property.Name]
      val processor = mock[A => B]("Processor")
      val definition = mock[PartialFunction[Property.Name, (Property.Name, A => B)]]("Process.Definition")
      val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")

      when(definition isDefinedAt input) thenReturn true
      when(definition apply input) thenReturn (output -> processor)
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
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.name should be(output)
          }
        }

        "has expected type" in {
          new Fixture {
            process(definition)
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.returns[B] should be(true)
          }
        }

        "when invoked" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "invoke the underlying origin" in {
            new Fixture {
              process(definition)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(underlying).apply(filter)
            }
          }

          "use the result of the underlying's origin as input for the processor" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              process(definition)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(processor).apply(value)
            }
          }

          "return the result of the processor" in {
            new Fixture {
              val value = new B
              when(processor(anything())) thenReturn value
              when(read()) thenReturn Some(new A)

              process(definition)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter).value.asInstanceOf[Property[B]]

              result.name should be(output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              process(definition)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter)

              verify(processor, never()).apply(anything())
              result should not be('defined)
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
          class C

          process(definition)
          origin.create(input)(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(definition, never()) isDefinedAt input
          verify(definition, never()) apply input
        }
      }
    }
  }

  "Map" when {

    trait Fixture {

      class A
      class B

      val input = random[Property.Name]
      val output = random[Property.Name]
      val mapper = mock[A => B]("Mapper")
      val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
    }

    "an underlying origin is created with matching name and type" should {
      "create an additional origin" which {
        "has expected name" in {
          new Fixture {
            map(input, output)(mapper)
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.name should be(output)
          }
        }

        "has expected type" in {
          new Fixture {
            map(input, output)(mapper)
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.returns[B] should be(true)
          }
        }

        "when invoked" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "invoke the underlying origin" in {
            new Fixture {
              map(input, output)(mapper)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(underlying).apply(filter)
            }
          }

          "use the result of the underlying's origin as input for the mapper" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              map(input, output)(mapper)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(mapper).apply(value)
            }
          }

          "return the result of the mapper" in {
            new Fixture {
              val value = new B
              when(mapper(anything())) thenReturn value
              when(read()) thenReturn Some(new A)

              map(input, output)(mapper)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter).value.asInstanceOf[Property[B]]

              result.name should be(output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              map(input, output)(mapper)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter)

              verify(mapper, never()).apply(anything())
              result should not be('defined)
            }
          }
        }
      }
    }

    "an origin is created with non-matching name" should {
      "not create an additional origin" in {
        new Fixture {
          map(input, output)(mapper)
          val underlying = origin.create(different(input))(read)

          verify(in(underlying.family), never()).create(equalTo(output))(anything())(anything(), anything())
        }
      }
    }

    "an origin is created with non-matching type" should {
      "not create an additional origin" in {
        new Fixture {
          class C

          map(input, output)(mapper)
          val underlying = origin.create(different(input))(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(in(underlying.family), never()).create(equalTo(output))(anything())(anything(), anything())
        }
      }
    }
  }

  "Operation" when {
    trait Fixture {

      class A
      class B

      val input = random[Property.Name]
      val output = random[Operation.Name]
      val function = mock[A => B]("Mapper")
      val read = mock[Origin.Read.Unfiltered[A]]("Origin.read")
    }

    "an underlying origin is created with matching type" should {
      "create an additional origin" which {
        "has expected name" in {
          new Fixture {
            operation(output)(function)
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.name should be(input / output)
          }
        }

        "has expected type" in {
          new Fixture {
            operation(output)(function)
            val underlying = origin.create(input)(read)

            families.find(underlying.family) should have size(2)
            families.find(underlying.family).last.returns[B] should be(true)
          }
        }

        "when invoked" should {
          val filter = mock[Filter[Origin.Meta.Readable]]("Filter")
          when(filter.apply(anything())) thenReturn true

          "invoke the underlying origin" in {
            new Fixture {
              operation(output)(function)
              val underlying = origin.create(input)(read)
              when(read()) thenReturn None

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(underlying).apply(filter)
            }
          }

          "use the result of the underlying's origin as input for the mapper" in {
            new Fixture {
              val value = new A
              when(read()) thenReturn Some(value)

              operation(output)(function)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              families.find(underlying.family).last(filter)

              verify(function).apply(value)
            }
          }

          "return the result of the mapper" in {
            new Fixture {
              val value = new B
              when(function(anything())) thenReturn value
              when(read()) thenReturn Some(new A)

              operation(output)(function)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter).value.asInstanceOf[Property[B]]

              result.name should be(input / output)
              result.value should be(value)
            }
          }

          "return None if the result of the underlying origin is not defined" in {
            new Fixture {
              when(read()) thenReturn None

              operation(output)(function)
              val underlying = origin.create(input)(read)

              families.find(underlying.family) should have size(2)
              val result = families.find(underlying.family).last(filter)

              verify(function, never()).apply(anything())
              result should not be('defined)
            }
          }
        }
      }
    }

    "an origin is created with non-matching type" should {
      "not create an additional origin" in {
        new Fixture {
          class C

          operation(output)(function)
          val underlying = origin.create(different(input))(mock[Origin.Read.Unfiltered[C]]("Origin.read"))

          verify(in(underlying.family), never()).create(equalTo(input / output))(anything())(anything(), anything())
        }
      }
    }
  }
}
