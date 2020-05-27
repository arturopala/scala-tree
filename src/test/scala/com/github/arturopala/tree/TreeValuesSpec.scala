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
      tree0.valuesWithFilter(all).toList shouldBe Nil
      tree1.valuesWithFilter(all).toList shouldBe List("a")
      tree2.valuesWithFilter(all).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valuesWithFilter(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valuesWithFilter(none).toList shouldBe Nil
      tree1.valuesWithFilter(none).toList shouldBe Nil
      tree2.valuesWithFilter(none).toList shouldBe Nil
      tree3_1.valuesWithFilter(none).toList shouldBe Nil
      tree3_2.valuesWithFilter(none).toList shouldBe Nil
      tree4_1.valuesWithFilter(none).toList shouldBe Nil
      tree4_2.valuesWithFilter(none).toList shouldBe Nil
      tree4_3.valuesWithFilter(none).toList shouldBe Nil
      tree7.valuesWithFilter(none).toList shouldBe Nil
      tree9.valuesWithFilter(none).toList shouldBe Nil

      tree0.valuesWithFilter(even).toList shouldBe Nil
      tree1.valuesWithFilter(even).toList shouldBe Nil
      tree2.valuesWithFilter(even).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even).toList shouldBe List("b", "d", "f", "h")

      tree0.valuesWithFilter(odd).toList shouldBe Nil
      tree1.valuesWithFilter(odd).toList shouldBe List("a")
      tree2.valuesWithFilter(odd).toList shouldBe List("a")
      tree3_1.valuesWithFilter(odd).toList shouldBe List("a", "c")
      tree3_2.valuesWithFilter(odd).toList shouldBe List("a", "c")
      tree4_1.valuesWithFilter(odd).toList shouldBe List("a", "c")
      tree4_2.valuesWithFilter(odd).toList shouldBe List("a", "c")
      tree4_3.valuesWithFilter(odd).toList shouldBe List("a", "c")
      tree7.valuesWithFilter(odd).toList shouldBe List("a", "c", "e", "g")
      tree9.valuesWithFilter(odd).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "iterate over nodes with filter and maxDepth" in {
      tree0.valuesWithFilter(all, 0).toList shouldBe Nil
      tree0.valuesWithFilter(all, 1).toList shouldBe Nil
      tree1.valuesWithFilter(all, 0).toList shouldBe Nil
      tree1.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree1.valuesWithFilter(all, 2).toList shouldBe List("a")
      tree2.valuesWithFilter(all, 0).toList shouldBe Nil
      tree2.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree2.valuesWithFilter(all, 2).toList shouldBe List("a", "b")
      tree2.valuesWithFilter(all, 3).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, 0).toList shouldBe Nil
      tree3_1.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree3_1.valuesWithFilter(all, 2).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c")
      tree3_1.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree3_2.valuesWithFilter(all, 2).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree4_1.valuesWithFilter(all, 2).toList shouldBe List("a", "b")
      tree4_1.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree4_2.valuesWithFilter(all, 2).toList shouldBe List("a", "b", "d")
      tree4_2.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree4_3.valuesWithFilter(all, 2).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all, 0).toList shouldBe Nil
      tree7.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree7.valuesWithFilter(all, 2).toList shouldBe List("a", "b", "d", "g")
      tree7.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c", "d", "e", "g")
      tree7.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valuesWithFilter(all, 0).toList shouldBe Nil
      tree9.valuesWithFilter(all, 1).toList shouldBe List("a")
      tree9.valuesWithFilter(all, 2).toList shouldBe List("a", "b", "e")
      tree9.valuesWithFilter(all, 3).toList shouldBe List("a", "b", "c", "e", "f", "h")
      tree9.valuesWithFilter(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
      tree9.valuesWithFilter(all, 5).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valuesWithFilter(none, 0).toList shouldBe Nil
      tree1.valuesWithFilter(none, 1).toList shouldBe Nil
      tree2.valuesWithFilter(none, 2).toList shouldBe Nil
      tree3_1.valuesWithFilter(none, 3).toList shouldBe Nil
      tree3_2.valuesWithFilter(none, 2).toList shouldBe Nil
      tree4_1.valuesWithFilter(none, 4).toList shouldBe Nil
      tree4_2.valuesWithFilter(none, 3).toList shouldBe Nil
      tree4_3.valuesWithFilter(none, 2).toList shouldBe Nil
      tree7.valuesWithFilter(none, 4).toList shouldBe Nil
      tree9.valuesWithFilter(none, 4).toList shouldBe Nil

      tree0.valuesWithFilter(even, 10).toList shouldBe Nil
      tree1.valuesWithFilter(even, 10).toList shouldBe Nil
      tree2.valuesWithFilter(even, 1).toList shouldBe Nil
      tree2.valuesWithFilter(even, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, 1).toList shouldBe Nil
      tree3_1.valuesWithFilter(even, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, 3).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, 1).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, 2).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, 1).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, 2).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, 4).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, 1).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, 2).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, 3).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, 1).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, 2).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, 1).toList shouldBe Nil
      tree7.valuesWithFilter(even, 2).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, 4).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even, 1).toList shouldBe Nil
      tree9.valuesWithFilter(even, 2).toList shouldBe List("b")
      tree9.valuesWithFilter(even, 3).toList shouldBe List("b", "f", "h")
      tree9.valuesWithFilter(even, 4).toList shouldBe List("b", "d", "f", "h")
      tree9.valuesWithFilter(even, 5).toList shouldBe List("b", "d", "f", "h")
    }

  }

}
