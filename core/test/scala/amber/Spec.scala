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

import scala.concurrent.duration._

import org.scalatest.{BeforeAndAfterEach, OptionValues, WordSpec}
import org.scalatest.matchers.ShouldMatchers

import amber.util.{Mocking, Randoms}

trait Spec extends WordSpec
           with BeforeAndAfterEach
           with ShouldMatchers
           with OptionValues
           with Mocking
           with Randoms {

  protected val timeout: FiniteDuration = 1.second
}
