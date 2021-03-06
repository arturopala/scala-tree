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

import scala.reflect.ClassTag

class TreeValuesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "get first child value" in {
      tree0.firstChildValue shouldBe None
      tree1.firstChildValue shouldBe None
      tree2.firstChildValue shouldBe Some("b")
      tree3_1.firstChildValue shouldBe Some("b")
      tree3_2.firstChildValue shouldBe Some("b")
      tree4_1.firstChildValue shouldBe Some("b")
      tree4_2.firstChildValue shouldBe Some("b")
      tree4_3.firstChildValue shouldBe Some("b")
      tree7.firstChildValue shouldBe Some("b")
      tree9.firstChildValue shouldBe Some("b")
      tree13.firstChildValue shouldBe Some("b")
    }

    "get first child" in {
      tree0.firstChild shouldBe None
      tree1.firstChild shouldBe None
      tree2.firstChild shouldBe Some(Tree("b"))
      tree3_1.firstChild shouldBe Some(Tree("b", Tree("c")))
      tree3_2.firstChild shouldBe Some(Tree("b"))
      tree4_1.firstChild shouldBe Some(Tree("b", Tree("c", Tree("d"))))
      tree4_2.firstChild shouldBe Some(Tree("b", Tree("c")))
      tree4_3.firstChild shouldBe Some(Tree("b"))
      tree7.firstChild shouldBe Some(Tree("b", Tree("c")))
      tree9.firstChild shouldBe Some(Tree("b", Tree("c", Tree("d"))))
      tree13.firstChild shouldBe
        Some(Tree("b", Tree("c", Tree("d")), Tree("e", Tree("f"), Tree("g")), Tree("h")))
    }

    "get last child value" in {
      tree0.lastChildValue shouldBe None
      tree1.lastChildValue shouldBe None
      tree2.lastChildValue shouldBe Some("b")
      tree3_1.lastChildValue shouldBe Some("b")
      tree3_2.lastChildValue shouldBe Some("c")
      tree4_1.lastChildValue shouldBe Some("b")
      tree4_2.lastChildValue shouldBe Some("d")
      tree4_3.lastChildValue shouldBe Some("d")
      tree7.lastChildValue shouldBe Some("g")
      tree9.lastChildValue shouldBe Some("e")
      tree13.lastChildValue shouldBe Some("j")
    }

    "get last child" in {
      tree0.lastChild shouldBe None
      tree1.lastChild shouldBe None
      tree2.lastChild shouldBe Some(Tree("b"))
      tree3_1.lastChild shouldBe Some(Tree("b", Tree("c")))
      tree3_2.lastChild shouldBe Some(Tree("c"))
      tree4_1.lastChild shouldBe Some(Tree("b", Tree("c", Tree("d"))))
      tree4_2.lastChild shouldBe Some(Tree("d"))
      tree4_3.lastChild shouldBe Some(Tree("d"))
      tree7.lastChild shouldBe Some(Tree("g"))
      tree9.lastChild shouldBe Some(Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree13.lastChild shouldBe Some(Tree("j", Tree("k", Tree("l")), Tree("m")))
    }

    "list all leaves" in {
      tree0.leaves shouldBe List()
      tree1.leaves shouldBe List("a")
      tree2.leaves shouldBe List("b")
      tree3_1.leaves shouldBe List("c")
      tree3_2.leaves.toList shouldBe List("b", "c")
      tree4_1.leaves shouldBe List("d")
      tree4_2.leaves shouldBe List("c", "d")
      tree4_3.leaves shouldBe List("b", "c", "d")
      tree7.leaves shouldBe List("c", "f", "g")
      tree9.leaves shouldBe List("d", "g", "i")
      tree13.leaves shouldBe List("d", "f", "g", "h", "i", "l", "m")
    }

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
      tree13.values(TopDownDepthFirst) shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
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
      tree13.values(TopDownBreadthFirst) shouldBe List("a", "b", "i", "j", "c", "e", "h", "k", "m", "d", "f", "g", "l")
    }

    "iterate over nodes with filter top-down and depth-first" in {
      tree0.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valuesWithFilter(all, TopDownDepthFirst).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree2.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_1.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_2.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_1.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_2.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_3.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree7.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree9.valuesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil

      tree0.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even, TopDownDepthFirst).toList shouldBe List("b", "d", "f", "h")

      tree0.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a")
      tree2.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a")
      tree3_1.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c")
      tree3_2.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c")
      tree4_1.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c")
      tree4_2.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c")
      tree4_3.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c")
      tree7.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c", "e", "g")
      tree9.valuesWithFilter(odd, TopDownDepthFirst).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "iterate over nodes with filter top-down and breadth-first" in {
      tree0.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a")
      tree2.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b")
      tree3_1.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "c")
      tree3_2.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "c")
      tree4_1.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "d", "c")
      tree4_3.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "c", "d")
      tree7.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "d", "g", "c", "e", "f")
      tree9.valuesWithFilter(all, TopDownBreadthFirst).toList shouldBe List("a", "b", "e", "c", "f", "h", "d", "g", "i")

      tree0.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree2.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_1.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_2.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_1.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_2.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_3.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree7.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree9.valuesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil

      tree0.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe Nil
      tree2.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b")
      tree3_1.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b")
      tree3_2.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b")
      tree4_1.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b", "d")
      tree4_2.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b", "d")
      tree4_3.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b", "d")
      tree7.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b", "d", "f")
      tree9.valuesWithFilter(even, TopDownBreadthFirst).toList shouldBe List("b", "f", "h", "d")

      tree0.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a")
      tree2.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a")
      tree3_1.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "c")
      tree3_2.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "c")
      tree4_1.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "c")
      tree4_2.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "c")
      tree4_3.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "c")
      tree7.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "g", "c", "e")
      tree9.valuesWithFilter(odd, TopDownBreadthFirst).toList shouldBe List("a", "e", "c", "g", "i")
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

    "iterate over values and levels with filter top-down and depth-first" in {
      tree0.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List((1, "a", true))
      tree2.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List((1, "a", false), (2, "b", true))
      tree3_1.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", true)
      )
      tree3_2.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", true),
        (2, "c", true)
      )
      tree4_1.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", false),
        (4, "d", true)
      )
      tree4_2.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", true),
        (2, "d", true)
      )
      tree4_3.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", true),
        (2, "c", true),
        (2, "d", true)
      )
      tree7.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", true),
        (2, "d", false),
        (3, "e", false),
        (4, "f", true),
        (2, "g", true)
      )
      tree9.valuesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", false),
        (4, "d", true),
        (2, "e", false),
        (3, "f", false),
        (4, "g", true),
        (3, "h", false),
        (4, "i", true)
      )

      tree0.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree2.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_1.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_2.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_1.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_2.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_3.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree7.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree9.valuesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil

      tree0.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe Nil
      tree2.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", true))
      tree3_1.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", false))
      tree3_2.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", true))
      tree4_1.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", false), (4, "d", true))
      tree4_2.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", false), (2, "d", true))
      tree4_3.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List((2, "b", true), (2, "d", true))
      tree7.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List(
        (2, "b", false),
        (2, "d", false),
        (4, "f", true)
      )
      tree9.valuesAndLevelsWithFilter(even, TopDownDepthFirst).toList shouldBe List(
        (2, "b", false),
        (4, "d", true),
        (3, "f", false),
        (3, "h", false)
      )

      tree0.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", true))
      tree2.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false))
      tree3_1.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false), (3, "c", true))
      tree3_2.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false), (2, "c", true))
      tree4_1.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false), (3, "c", false))
      tree4_2.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false), (3, "c", true))
      tree4_3.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List((1, "a", false), (2, "c", true))
      tree7.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (3, "c", true),
        (3, "e", false),
        (2, "g", true)
      )
      tree9.valuesAndLevelsWithFilter(odd, TopDownDepthFirst).toList shouldBe List(
        (1, "a", false),
        (3, "c", false),
        (2, "e", false),
        (4, "g", true),
        (4, "i", true)
      )
    }

    "iterate over values and levels with filter top-down and breadth-first" in {
      tree0.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List((1, "a", true))
      tree2.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (2, "b", true))
      tree3_1.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", true)
      )
      tree3_2.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", true),
        (2, "c", true)
      )
      tree4_1.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (3, "c", false),
        (4, "d", true)
      )
      tree4_2.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (2, "d", true),
        (3, "c", true)
      )
      tree4_3.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", true),
        (2, "c", true),
        (2, "d", true)
      )
      tree7.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (2, "d", false),
        (2, "g", true),
        (3, "c", true),
        (3, "e", false),
        (4, "f", true)
      )
      tree9.valuesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "b", false),
        (2, "e", false),
        (3, "c", false),
        (3, "f", false),
        (3, "h", false),
        (4, "d", true),
        (4, "g", true),
        (4, "i", true)
      )

      tree0.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree2.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_1.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_2.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_1.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_2.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_3.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree7.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree9.valuesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil

      tree0.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe Nil
      tree2.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", true))
      tree3_1.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", false))
      tree3_2.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", true))
      tree4_1.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", false), (4, "d", true))
      tree4_2.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", false), (2, "d", true))
      tree4_3.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List((2, "b", true), (2, "d", true))
      tree7.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List(
        (2, "b", false),
        (2, "d", false),
        (4, "f", true)
      )
      tree9.valuesAndLevelsWithFilter(even, TopDownBreadthFirst).toList shouldBe List(
        (2, "b", false),
        (3, "f", false),
        (3, "h", false),
        (4, "d", true)
      )

      tree0.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe Nil
      tree1.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", true))
      tree2.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false))
      tree3_1.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (3, "c", true))
      tree3_2.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (2, "c", true))
      tree4_1.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (3, "c", false))
      tree4_2.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (3, "c", true))
      tree4_3.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List((1, "a", false), (2, "c", true))
      tree7.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "g", true),
        (3, "c", true),
        (3, "e", false)
      )
      tree9.valuesAndLevelsWithFilter(odd, TopDownBreadthFirst).toList shouldBe List(
        (1, "a", false),
        (2, "e", false),
        (3, "c", false),
        (4, "g", true),
        (4, "i", true)
      )
    }

  }

}
