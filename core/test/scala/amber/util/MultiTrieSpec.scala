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
package util

import scala.collection.immutable.Set

class MultiTrieSpec extends Spec {

  type Selection = MultiTrie.Selection
  import MultiTrie.Selections

  class A

  trait Fixture {
    val trie = MultiTrie[A]()
    val value = new A
  }

  "MultiTrie" should {
    "select an added value" in {
      new Fixture {
        val path = random[Path]

        trie.add(path, value)

        trie.select(Selections.exact(path)) should contain(value)
      }
    }
  }

  "Selections.all" should {
    "select any added value" in {
      new Fixture {
        trie.add(random[Path], value)

        trie.select(Selections.all) should contain(value)
      }
    }
  }

  "Selections.exact" should {
    selection.test(Selections.exact) { selects =>
      selects.value { using =>
        using.samePath()
      }

      selects.nothing { using =>
        using.differentPath()
        using.parentPath()
        using.grandparentPath()
        using.childPath()
        using.grandchildPath()
      }
    }
  }

  "Selections.childrenOf" should {
    selection.test(Selections.childrenOf) { selects =>
      selects.value { using =>
        using.parentPath()
      }

      selects.nothing { using =>
        using.samePath()
        using.differentPath()
        using.grandparentPath()
        using.childPath()
        using.grandchildPath()
      }
    }
  }

  "Selections.descendantsOf" should {
    selection.test(Selections.descendantsOf) { selects =>
      selects.value { using =>
        using.parentPath()
        using.grandparentPath()
      }

      selects.nothing { using =>
        using.samePath()
        using.differentPath()
        using.childPath()
        using.grandchildPath()
      }
    }
  }

  "Selections.parentOf" should {
    selection.test(Selections.parentOf) { selects =>
      selects.value { using =>
        using.childPath()
      }

      selects.nothing { using =>
        using.samePath()
        using.differentPath()
        using.parentPath()
        using.grandparentPath()
        using.grandchildPath()
      }
    }
  }

  "Selections.ancestorsOf" should {
    selection.test(Selections.ancestorsOf) { selects =>
      selects.value { using =>
        using.childPath()
        using.grandchildPath()
      }

      selects.nothing { using =>
        using.samePath()
        using.differentPath()
        using.parentPath()
        using.grandparentPath()
      }
    }
  }

  object selection {
    def test(builder: Path => Selection)(f: Selects => Unit) {f(new Selects {
      override def create(path: Path) = builder(path)
    })}
  }

  trait Selects {

    def create(path: Path): Selection

    def value(f: Using => Unit) {
      "select a value" when {
        f(new Using {
          override def create(path: Path) = Selects.this.create(path)
          override def check(as: Set[A], a: A) {as should contain(a)}
        })
      }
    }

    def nothing(f: Using => Unit) {
      "not select a value" when {
        f(new Using {
          override def create(path: Path) = Selects.this.create(path)
          override def check(as: Set[A], a: A) {as should not contain(a)}
        })
      }
    }
  }

  trait Using {

    def create(path: Path): Selection
    def check(as: Set[A], a: A)

    def samePath() {
      "using the same path" in {
        new Fixture {
          val path = random[Path]

          trie.add(path, value)

          check(trie.select(create(path)), value)
        }
      }
    }

    def differentPath() {
      "using a different path" in {
        new Fixture {
          val path = random[Path]

          trie.add(path, value)

          check(trie.select(create(different(path))), value)
        }
      }
    }

    def parentPath() {
      "using a parent path" in {
        new Fixture {
          val parent = random[Path]
          val path = parent / random[String]

          trie.add(path, value)

          check(trie.select(create(parent)), value)
        }
      }
    }

    def grandparentPath() {
      "using an ancestor path" in {
        new Fixture {
          val ancestor = random[Path]
          val path = ancestor / random[String] / random[String]

          trie.add(path, value)

          check(trie.select(create(ancestor)), value)
        }
      }
    }

    def childPath() {
      "using a child path" in {
        new Fixture {
          val path = random[Path]
          val child = path / random[String]

          trie.add(path, value)

          check(trie.select(create(child)), value)
        }
      }
    }

    def grandchildPath() {
      "using a descendant path" in {
        new Fixture {
          val path = random[Path]
          val descendant = path / random[String] / random[String]

          trie.add(path, value)

          check(trie.select(create(descendant)), value)
        }
      }
    }
  }
}
