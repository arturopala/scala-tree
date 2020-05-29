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

import com.github.arturopala.tree.TreeOptions.TraversingMode.{TopDownBreadthFirst, TopDownDepthFirst}

class TreeValuesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "list all nodes top-down and depth-first" in {
      tree0.values(TopDownDepthFirst) shouldBe Nil
      tree1.values(TopDownDepthFirst) shouldBe List("a")
      tree2.values(TopDownDepthFirst) shouldBe List("a", "b")
      tree3_1.values(TopDownDepthFirst) shouldBe List("a", "b", "c")
      tree3_2.values(TopDownDepthFirst) shouldBe List("a", "b", "c")
      tree4_1.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d")
      tree4_2.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d")
      tree4_3.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d")
      tree7.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "list all nodes top-down and breadth-first" in {
      tree0.values(TopDownBreadthFirst) shouldBe Nil
      tree1.values(TopDownBreadthFirst) shouldBe List("a")
      tree2.values(TopDownBreadthFirst) shouldBe List("a", "b")
      tree3_1.values(TopDownBreadthFirst) shouldBe List("a", "b", "c")
      tree3_2.values(TopDownBreadthFirst) shouldBe List("a", "b", "c")
      tree4_1.values(TopDownBreadthFirst) shouldBe List("a", "b", "c", "d")
      tree4_2.values(TopDownBreadthFirst) shouldBe List("a", "b", "d", "c")
      tree4_3.values(TopDownBreadthFirst) shouldBe List("a", "b", "c", "d")
      tree7.values(TopDownBreadthFirst) shouldBe List("a", "b", "d", "g", "c", "e", "f")
      tree9.values(TopDownBreadthFirst) shouldBe List("a", "b", "e", "c", "f", "h", "d", "g", "i")
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

    "iterate top-down, depth-first, over nodes with filter and maxDepth" in {
      tree0.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree0.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree1.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree2.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b")
      tree2.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree3_1.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree3_1.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c")
      tree3_1.valuesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree3_2.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree4_1.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b")
      tree4_1.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree4_2.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b", "d")
      tree4_2.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree4_3.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree7.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree7.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b", "d", "g")
      tree7.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c", "d", "e", "g")
      tree7.valuesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valuesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree9.valuesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List("a")
      tree9.valuesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List("a", "b", "e")
      tree9.valuesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List("a", "b", "c", "e", "f", "h")
      tree9
        .valuesWithFilter(all, TopDownDepthFirst, 4)
        .toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
      tree9
        .valuesWithFilter(all, TopDownDepthFirst, 5)
        .toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valuesWithFilter(none, TopDownDepthFirst, 0).toList shouldBe Nil
      tree1.valuesWithFilter(none, TopDownDepthFirst, 1).toList shouldBe Nil
      tree2.valuesWithFilter(none, TopDownDepthFirst, 2).toList shouldBe Nil
      tree3_1.valuesWithFilter(none, TopDownDepthFirst, 3).toList shouldBe Nil
      tree3_2.valuesWithFilter(none, TopDownDepthFirst, 2).toList shouldBe Nil
      tree4_1.valuesWithFilter(none, TopDownDepthFirst, 4).toList shouldBe Nil
      tree4_2.valuesWithFilter(none, TopDownDepthFirst, 3).toList shouldBe Nil
      tree4_3.valuesWithFilter(none, TopDownDepthFirst, 2).toList shouldBe Nil
      tree7.valuesWithFilter(none, TopDownDepthFirst, 4).toList shouldBe Nil
      tree9.valuesWithFilter(none, TopDownDepthFirst, 4).toList shouldBe Nil

      tree0.valuesWithFilter(even, TopDownDepthFirst, 10).toList shouldBe Nil
      tree1.valuesWithFilter(even, TopDownDepthFirst, 10).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree3_1.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownDepthFirst, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownDepthFirst, 4).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree7.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownDepthFirst, 4).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even, TopDownDepthFirst, 1).toList shouldBe Nil
      tree9.valuesWithFilter(even, TopDownDepthFirst, 2).toList shouldBe List("b")
      tree9.valuesWithFilter(even, TopDownDepthFirst, 3).toList shouldBe List("b", "f", "h")
      tree9.valuesWithFilter(even, TopDownDepthFirst, 4).toList shouldBe List("b", "d", "f", "h")
      tree9.valuesWithFilter(even, TopDownDepthFirst, 5).toList shouldBe List("b", "d", "f", "h")
    }

    "iterate top-down, breadth-first, over nodes with filter and maxDepth" in {
      tree0.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree0.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree1.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree2.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b")
      tree2.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "c")
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b")
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b", "d")
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "d", "c")
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List("a", "b", "d", "c")
      tree4_3.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree4_3.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree7.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree7.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b", "d", "g")
      tree7.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "d", "g", "c", "e")
      tree7.valuesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List("a", "b", "d", "g", "c", "e", "f")
      tree9.valuesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree9.valuesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List("a")
      tree9.valuesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List("a", "b", "e")
      tree9.valuesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List("a", "b", "e", "c", "f", "h")
      tree9
        .valuesWithFilter(all, TopDownBreadthFirst, 4)
        .toList shouldBe List("a", "b", "e", "c", "f", "h", "d", "g", "i")
      tree9
        .valuesWithFilter(all, TopDownBreadthFirst, 5)
        .toList shouldBe List("a", "b", "e", "c", "f", "h", "d", "g", "i")

      tree0.valuesWithFilter(none, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree1.valuesWithFilter(none, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree2.valuesWithFilter(none, TopDownBreadthFirst, 2).toList shouldBe Nil
      tree3_1.valuesWithFilter(none, TopDownBreadthFirst, 3).toList shouldBe Nil
      tree3_2.valuesWithFilter(none, TopDownBreadthFirst, 2).toList shouldBe Nil
      tree4_1.valuesWithFilter(none, TopDownBreadthFirst, 4).toList shouldBe Nil
      tree4_2.valuesWithFilter(none, TopDownBreadthFirst, 3).toList shouldBe Nil
      tree4_3.valuesWithFilter(none, TopDownBreadthFirst, 2).toList shouldBe Nil
      tree7.valuesWithFilter(none, TopDownBreadthFirst, 4).toList shouldBe Nil
      tree9.valuesWithFilter(none, TopDownBreadthFirst, 4).toList shouldBe Nil

      tree0.valuesWithFilter(even, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree1.valuesWithFilter(even, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree3_1.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree3_2.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst, 4).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree4_2.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree4_3.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree7.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownBreadthFirst, 4).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree9.valuesWithFilter(even, TopDownBreadthFirst, 2).toList shouldBe List("b")
      tree9.valuesWithFilter(even, TopDownBreadthFirst, 3).toList shouldBe List("b", "f", "h")
      tree9.valuesWithFilter(even, TopDownBreadthFirst, 4).toList shouldBe List("b", "f", "h", "d")
      tree9.valuesWithFilter(even, TopDownBreadthFirst, 5).toList shouldBe List("b", "f", "h", "d")
    }

  }

}
