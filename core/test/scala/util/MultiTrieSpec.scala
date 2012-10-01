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
    val trie = MultiTrie[String]()
    val value = random[String]
  }

  "MultiTrie" when {
    "a value is added using a path" should {
      "be able to get the value" when {
        "using the same path" in {
          new Fixture {
            val path = random[Path]

            trie.add(path, value)

            trie.find(path) should contain(value)
          }
        }

        "using a parent path" in {
          new Fixture {
            val parent = random[Path]
            val path = parent / random[String]

            trie.add(path, value)

            trie.find(parent) should contain(value)
          }
        }

        "using root path" in {
          new Fixture {
            trie.add(random[Path], value)

            trie.find(Path.Root) should contain(value)
          }
        }
      }

      "not be able to get the value" when {
        "using a different path" in {
          new Fixture {
            val path = random[Path]

            trie.add(path, value)

            trie.find(different(path)) should not(contain(value))
          }
        }

        "using a child path" in {
          new Fixture {
            val path = random[Path]
            val child = path / random[String]

            trie.add(path, value)

            trie.find(child) should not(contain(value))
          }
        }
      }
    }
  }
}
