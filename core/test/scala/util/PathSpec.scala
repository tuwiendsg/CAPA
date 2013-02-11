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
package util

class PathSpec extends Spec {

  trait Fixture {
    val path = random[Path]
  }

  "Path" when {
    "/ method is invoked" should {
      "result in a new path" which {
        "has the method's argument appended to it" in {
          new Fixture {
            val suffix = random[String]

            (path / suffix).toString should be(s"$path/$suffix")
          }
        }
      }
    }

    ">:> method is invoked" should {
      "return true if the method's argument is the same path" in {
        new Fixture {
          (path >:> path) should be(true)
        }
      }

      "return true if the method's argument is a child path" in {
        new Fixture {
          val child = path / random[String]

          (path >:> child) should be(true)
        }
      }

      "return false if the method's argument is a different path" in {
        new Fixture {
          (path >:> different(path)) should be(false)
        }
      }

      "return false if the method's argument is a parent path" in {
        new Fixture {
          val parent = path / random[String]

          (parent >:> path) should be(false)
        }
      }
    }
  }
}
