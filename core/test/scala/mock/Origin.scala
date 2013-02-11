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
package mock

import org.mockito.Matchers.{anyObject => anything}
import org.mockito.Mockito.when

import org.scalatest.mock.MockitoSugar.mock

import amber.util.{Filter, Mocking, Randoms}

object Origin extends Mocking with Randoms {

  def apply[A <: AnyRef : Manifest, B: amber.Origin.Read[A]#apply](name: Property.Name,
                                                                   family: Family,
                                                                   read: B): amber.Origin[A] = {
    val origin = mock[amber.Origin[A]]("mock.Origin[" + manifest[A] + "]")
    val meta = amber.Origin.Meta.Writable()

    when(origin.name) thenReturn name
    when(origin.family) thenReturn family
    when(origin.meta) thenReturn meta
    when(origin.returns(anything(), anything())) thenAnswer {
      args: Array[AnyRef] => manifest[A] <:< args(1).asInstanceOf[Manifest[_ <: AnyRef]]
    }
    when(origin.apply(anything())) thenAnswer {
      args: Array[AnyRef] =>
        val filter = args(0).asInstanceOf[Filter[amber.Origin.Meta.Readable]]
        (read match {
          case f: amber.Origin.Read.Unfiltered[A] => if (filter(meta)) f() else None
          case f: amber.Origin.Read.Filtered[A] =>
            f(args(0).asInstanceOf[Filter[amber.Origin.Meta.Readable]])
        }) map {Property(name, _)}
    }

    origin
  }

  def apply(name: Property.Name = random[Property.Name],
            family: Family = random[Family]): amber.Origin[AnyRef] =
    apply[AnyRef, amber.Origin.Read.Unfiltered[AnyRef]](name, family, () => None)

  def create[A <: AnyRef : Manifest](name: Property.Name)
                                    (read: amber.Origin.Read.Unfiltered[A]): amber.Origin[A] =
    apply(name, random[Family], read)

  def create[A <: AnyRef : Manifest](name: Property.Name, family: Family)
                                    (read: amber.Origin.Read.Filtered[A]): amber.Origin[A] =
    apply(name, family, read)
}
