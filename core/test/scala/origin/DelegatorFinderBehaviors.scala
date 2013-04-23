/*
 * Copyright 2013 Sanjin Sehic
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
package origin

import scala.language.higherKinds

import scala.concurrent.Future

import scalaz.Id.Id

import org.mockito.Matchers.{anyObject => anything, eq => equalTo}
import org.mockito.Mockito.verify

import util.Type

trait DelegatorFinderBehaviors[X[+_]] {
  this: Spec with FinderComponent.Delegator[X] =>

  class A

  def aDelegator {
    "invoke the delegatee's find method" in {
      val selection = Selections.all

      origins.find[A](selection)

      verify(finder.origins).find(equalTo(selection))(anything(), equalTo(Type[A]))
    }
  }
}

object DelegatorFinderBehaviors {

  trait Local extends DelegatorFinderBehaviors[Id] {
    this: Spec with FinderComponent.Delegator.Local =>
  }

  trait Remote extends DelegatorFinderBehaviors[Future] {
    this: Spec with FinderComponent.Delegator.Remote =>
  }
}
