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
package mock.origin

import scala.language.higherKinds

import scala.collection.immutable.List

import scalaz.syntax.functor._

import org.scalatest.BeforeAndAfterEach

import util.{Mocker, Mocking, Type}

trait BuilderComponent extends amber.origin.BuilderComponent with Mocking {

  def mocker[A: Type]: Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]]

  var built = List.empty[Origin[_]]
  var build = mock[(Origin.Name, Origin.Family) => Unit]("mock.OriginBuilder.build")

  override protected type Origin[+A] <: amber.Origin[A]
  override protected def builder: OriginBuilder = _builder

  private object _builder extends OriginBuilder {

    override def build[A: Type](name: Origin.Name, family: Origin.Family)
                               (read: OriginBuilder.Read[A]) = {
      val origin = mocker[A].mock((name, family, read))
      built = built :+ origin
      BuilderComponent.this.build(name, family)

      origin
    }

    override def map[A, B: Type](underlying: Origin[A], name: Origin.Name)(f: A => B) = {
      val origin = mocker[B].mock((name, underlying.family, {meta =>
        underlying.read().asInstanceOf[Reading[(Origin.Value[A], Origin.MetaInfo)]] map {
          case (Origin.Value(_, value), other) => (Origin.Value(name, f(value)), meta :+ other)
        }
      }))
      built = built :+ origin
      BuilderComponent.this.build(name, underlying.family)

      origin
    }
  }
}

object BuilderComponent {

  trait InSpec extends BeforeAndAfterEach {
    this: Spec with BuilderComponent =>

    override def beforeEach() {
      built = List.empty
      build = mock[(Origin.Name, Origin.Family) => Unit]("mock.OriginBuilder.build")

      super.beforeEach()
    }
  }

  trait Local extends amber.origin.BuilderComponent.Local with BuilderComponent {
    override protected type Origin[+A] <: Origin.Local[A]
  }

  trait Remote extends amber.origin.BuilderComponent.Remote with BuilderComponent {
    override protected type Origin[+A] <: Origin.Remote[A]
  }

  object Local {
    trait Default extends BuilderComponent.Local {

      override protected type Origin[+A] = Origin.Local[A]

      override def mocker[A](implicit typeA: Type[A]) =
        new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]] {
          def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A])) =
            Default.this.mock[Origin[A]](s"mock.Origin.Local[$typeA]")
        }
    }
  }

  object Remote {
    trait Default extends BuilderComponent.Remote {

      override protected type Origin[+A] = Origin.Remote[A]

      override def mocker[A](implicit typeA: Type[A]) =
        new Mocker[(Origin.Name, Origin.Family, OriginBuilder.Read[A]), Origin[A]] {
          def mock(args: (Origin.Name, Origin.Family, OriginBuilder.Read[A])) =
            Default.this.mock[Origin[A]](s"mock.Origin.Remote[$typeA]")
        }
    }
  }
}
