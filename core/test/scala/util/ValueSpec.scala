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
package util

class ValueSpec extends Spec {

  class O
  class A extends O
  class U extends A
  class B

  trait Fixture {
    val a = new A
    val value = new Value(a)
  }

  "Value" when {
    "return the value if asked for a same type" in {
      new Fixture {
        value.as[A].value should be(a)
      }
    }

    "return the value if asked for a super type" in {
      new Fixture {
        value.as[O].value should be(a)
      }
    }

    "return None if asked for a different type" in {
      new Fixture {
        value.as[B] should be(None)
      }
    }

    "return None if asked for a sub type" in {
      new Fixture {
        value.as[U] should be(None)
      }
    }
  }
}
