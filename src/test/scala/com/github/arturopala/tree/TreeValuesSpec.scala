/*
 * Copyright 2020 Artur Opala
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

package com.github.arturopala.tree

class TreeValuesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "list all nodes" in {
      tree0.values shouldBe Nil
      tree1.values shouldBe List("a")
      tree2.values shouldBe List("a", "b")
      tree3_1.values shouldBe List("a", "b", "c")
      tree3_2.values shouldBe List("a", "b", "c")
      tree4_1.values shouldBe List("a", "b", "c", "d")
      tree4_2.values shouldBe List("a", "b", "c", "d")
      tree4_3.values shouldBe List("a", "b", "c", "d")
      tree7.values shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.values shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "iterate over nodes with filter" in {
      tree0.valueIterator(all).toList shouldBe Nil
      tree1.valueIterator(all).toList shouldBe List("a")
      tree2.valueIterator(all).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree7.valueIterator(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueIterator(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valueIterator(none).toList shouldBe Nil
      tree1.valueIterator(none).toList shouldBe Nil
      tree2.valueIterator(none).toList shouldBe Nil
      tree3_1.valueIterator(none).toList shouldBe Nil
      tree3_2.valueIterator(none).toList shouldBe Nil
      tree4_1.valueIterator(none).toList shouldBe Nil
      tree4_2.valueIterator(none).toList shouldBe Nil
      tree4_3.valueIterator(none).toList shouldBe Nil
      tree7.valueIterator(none).toList shouldBe Nil
      tree9.valueIterator(none).toList shouldBe Nil

      tree0.valueIterator(even).toList shouldBe Nil
      tree1.valueIterator(even).toList shouldBe Nil
      tree2.valueIterator(even).toList shouldBe List("b")
      tree3_1.valueIterator(even).toList shouldBe List("b")
      tree3_2.valueIterator(even).toList shouldBe List("b")
      tree4_1.valueIterator(even).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even).toList shouldBe List("b", "d")
      tree7.valueIterator(even).toList shouldBe List("b", "d", "f")
      tree9.valueIterator(even).toList shouldBe List("b", "d", "f", "h")

      tree0.valueIterator(odd).toList shouldBe Nil
      tree1.valueIterator(odd).toList shouldBe List("a")
      tree2.valueIterator(odd).toList shouldBe List("a")
      tree3_1.valueIterator(odd).toList shouldBe List("a", "c")
      tree3_2.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_1.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_2.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_3.valueIterator(odd).toList shouldBe List("a", "c")
      tree7.valueIterator(odd).toList shouldBe List("a", "c", "e", "g")
      tree9.valueIterator(odd).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "iterate over nodes with filter and maxDepth" in {
      tree0.valueIterator(all, 0).toList shouldBe Nil
      tree0.valueIterator(all, 1).toList shouldBe Nil
      tree1.valueIterator(all, 0).toList shouldBe Nil
      tree1.valueIterator(all, 1).toList shouldBe List("a")
      tree1.valueIterator(all, 2).toList shouldBe List("a")
      tree2.valueIterator(all, 0).toList shouldBe Nil
      tree2.valueIterator(all, 1).toList shouldBe List("a")
      tree2.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree2.valueIterator(all, 3).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all, 0).toList shouldBe Nil
      tree3_1.valueIterator(all, 1).toList shouldBe List("a")
      tree3_1.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree3_1.valueIterator(all, 4).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 0).toList shouldBe Nil
      tree3_2.valueIterator(all, 1).toList shouldBe List("a")
      tree3_2.valueIterator(all, 2).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 4).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all, 0).toList shouldBe Nil
      tree4_1.valueIterator(all, 1).toList shouldBe List("a")
      tree4_1.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree4_1.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all, 0).toList shouldBe Nil
      tree4_2.valueIterator(all, 1).toList shouldBe List("a")
      tree4_2.valueIterator(all, 2).toList shouldBe List("a", "b", "d")
      tree4_2.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all, 0).toList shouldBe Nil
      tree4_3.valueIterator(all, 1).toList shouldBe List("a")
      tree4_3.valueIterator(all, 2).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree7.valueIterator(all, 0).toList shouldBe Nil
      tree7.valueIterator(all, 1).toList shouldBe List("a")
      tree7.valueIterator(all, 2).toList shouldBe List("a", "b", "d", "g")
      tree7.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d", "e", "g")
      tree7.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueIterator(all, 0).toList shouldBe Nil
      tree9.valueIterator(all, 1).toList shouldBe List("a")
      tree9.valueIterator(all, 2).toList shouldBe List("a", "b", "e")
      tree9.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "e", "f", "h")
      tree9.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
      tree9.valueIterator(all, 5).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valueIterator(none, 0).toList shouldBe Nil
      tree1.valueIterator(none, 1).toList shouldBe Nil
      tree2.valueIterator(none, 2).toList shouldBe Nil
      tree3_1.valueIterator(none, 3).toList shouldBe Nil
      tree3_2.valueIterator(none, 2).toList shouldBe Nil
      tree4_1.valueIterator(none, 4).toList shouldBe Nil
      tree4_2.valueIterator(none, 3).toList shouldBe Nil
      tree4_3.valueIterator(none, 2).toList shouldBe Nil
      tree7.valueIterator(none, 4).toList shouldBe Nil
      tree9.valueIterator(none, 4).toList shouldBe Nil

      tree0.valueIterator(even, 10).toList shouldBe Nil
      tree1.valueIterator(even, 10).toList shouldBe Nil
      tree2.valueIterator(even, 1).toList shouldBe Nil
      tree2.valueIterator(even, 2).toList shouldBe List("b")
      tree3_1.valueIterator(even, 1).toList shouldBe Nil
      tree3_1.valueIterator(even, 2).toList shouldBe List("b")
      tree3_1.valueIterator(even, 3).toList shouldBe List("b")
      tree3_2.valueIterator(even, 0).toList shouldBe Nil
      tree3_2.valueIterator(even, 1).toList shouldBe Nil
      tree3_2.valueIterator(even, 2).toList shouldBe List("b")
      tree3_2.valueIterator(even, 3).toList shouldBe List("b")
      tree4_1.valueIterator(even, 0).toList shouldBe Nil
      tree4_1.valueIterator(even, 1).toList shouldBe Nil
      tree4_1.valueIterator(even, 2).toList shouldBe List("b")
      tree4_1.valueIterator(even, 3).toList shouldBe List("b")
      tree4_1.valueIterator(even, 4).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even, 0).toList shouldBe Nil
      tree4_2.valueIterator(even, 1).toList shouldBe Nil
      tree4_2.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even, 0).toList shouldBe Nil
      tree4_3.valueIterator(even, 1).toList shouldBe Nil
      tree4_3.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 1).toList shouldBe Nil
      tree7.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 4).toList shouldBe List("b", "d", "f")
      tree9.valueIterator(even, 1).toList shouldBe Nil
      tree9.valueIterator(even, 2).toList shouldBe List("b")
      tree9.valueIterator(even, 3).toList shouldBe List("b", "f", "h")
      tree9.valueIterator(even, 4).toList shouldBe List("b", "d", "f", "h")
      tree9.valueIterator(even, 5).toList shouldBe List("b", "d", "f", "h")
    }

  }

}
