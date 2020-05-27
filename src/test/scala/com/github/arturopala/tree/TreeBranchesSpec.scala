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

class TreeBranchesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "list all branches" in {
      tree0.branches.map(_.toList).toList shouldBe Nil
      tree1.branches.map(_.toList).toList shouldBe List(List("a"))
      tree2.branches.map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branches.map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branches.map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branches.map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branches.map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branches.map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branches.map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      tree9.branches.map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
    }

    "iterate over branches with filter" in {
      tree0.branchesWithFilter(all).map(_.toList).toList shouldBe Nil
      tree1.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a"))
      tree2.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchesWithFilter(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branchesWithFilter(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchesWithFilter(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      tree0.branchesWithFilter(none).toList shouldBe Nil
      tree1.branchesWithFilter(none).toList shouldBe Nil
      tree2.branchesWithFilter(none).toList shouldBe Nil
      tree3_1.branchesWithFilter(none).toList shouldBe Nil
      tree3_2.branchesWithFilter(none).toList shouldBe Nil
      tree4_1.branchesWithFilter(none).toList shouldBe Nil
      tree4_2.branchesWithFilter(none).toList shouldBe Nil
      tree4_3.branchesWithFilter(none).toList shouldBe Nil
      tree7.branchesWithFilter(none).toList shouldBe Nil
      tree9.branchesWithFilter(none).toList shouldBe Nil

      tree0.branchesWithFilter(_.size > 3).toList shouldBe Nil
      tree1.branchesWithFilter(_.size > 1).toList shouldBe Nil
      tree2.branchesWithFilter(_.size > 1).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(_.last == "c").map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(_.last == "c").map(_.toList).toList shouldBe List(List("a", "c"))
      tree4_2.branchesWithFilter(_.last == "d").map(_.toList).toList shouldBe List(List("a", "d"))
      tree4_2.branchesWithFilter(_.size > 2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree7.branchesWithFilter(_.size > 3).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
      tree9.branchesWithFilter(_.size > 3).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchesWithFilter(_.toList.contains("e")).map(_.toList).toList shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchesWithFilter(_.toList.contains("h")).map(_.toList).toList shouldBe List(List("a", "e", "h", "i"))
      tree9.branchesWithFilter(_.size < 3).map(_.toList).toList shouldBe Nil
    }

    "iterate over branches with filter and depth limit" in {
      tree0.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree0.branchesWithFilter(all, 1).map(_.toList).toList shouldBe Nil
      tree1.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree1.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree1.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a"))
      tree2.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree2.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree2.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"))
      tree2.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree3_1.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree3_1.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_1.branchesWithFilter(all, 4).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree3_2.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree3_2.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree3_2.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree4_1.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree4_1.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"))
      tree4_1.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree4_1.branchesWithFilter(all, 4).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree4_2.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree4_2.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "d"))
      tree4_2.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_2.branchesWithFilter(all, 4).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree4_3.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree4_3.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      tree4_3.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      tree7.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree7.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree7.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(
        List("a", "b"),
        List("a", "d"),
        List("a", "g")
      )
      tree7.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e"),
        List("a", "g")
      )
      tree7.branchesWithFilter(all, 4).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchesWithFilter(all, 0).map(_.toList).toList shouldBe Nil
      tree9.branchesWithFilter(all, 1).map(_.toList).toList shouldBe List(List("a"))
      tree9.branchesWithFilter(all, 2).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "e"))
      tree9.branchesWithFilter(all, 3).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "e", "f"),
        List("a", "e", "h")
      )
      tree9.branchesWithFilter(all, 4).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      tree0.branchesWithFilter(none, 0).toList shouldBe Nil
      tree1.branchesWithFilter(none, 1).toList shouldBe Nil
      tree2.branchesWithFilter(none, 2).toList shouldBe Nil
      tree3_1.branchesWithFilter(none, 3).toList shouldBe Nil
      tree3_2.branchesWithFilter(none, 2).toList shouldBe Nil
      tree4_1.branchesWithFilter(none, 4).toList shouldBe Nil
      tree4_2.branchesWithFilter(none, 3).toList shouldBe Nil
      tree4_3.branchesWithFilter(none, 2).toList shouldBe Nil
      tree7.branchesWithFilter(none, 4).toList shouldBe Nil
      tree9.branchesWithFilter(none, 4).toList shouldBe Nil
    }

  }

}
