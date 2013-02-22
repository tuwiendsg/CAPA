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

class MultiTrieSpec extends Spec {

  trait Fixture {
    val trie = MultiTrie[Origin.Name, String]()
    val value = random[String]
  }

  "MultiTrie" when {
    "a value is added using a key" should {
      "be able to get the value" when {
        "using the same key" in {
          new Fixture {
            val key = random[Origin.Name]

            trie += (Some(key), value)

            trie(Some(key)) should contain(value)
          }
        }

        "using a parent key" in {
          new Fixture {
            val superKey = random[Origin.Name]
            val key = superKey / random[String]

            trie += (Some(key), value)

            trie(Some(superKey)) should contain(value)
          }
        }

        "using no key" in {
          new Fixture {
            trie += (Some(random[Origin.Name]), value)

            trie(None) should contain(value)
          }
        }
      }

      "not be able to get the value" when {
        "using a different key" in {
          new Fixture {
            val key = random[Origin.Name]

            trie += (Some(key), value)

            trie(Some(different(key))) should not(contain(value))
          }
        }

        "using a child key" in {
          new Fixture {
            val key = random[Origin.Name]
            val subKey = key / random[String]

            trie += (Some(key), value)

            trie(Some(subKey)) should not(contain(value))
          }
        }
      }
    }
  }
}
