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

    "iterate all paths" in {
      tree0.paths shouldBe Nil
      tree1.paths shouldBe List(List("a"))
      tree2.paths shouldBe List(List("a"), List("a", "b"))
      tree3_1.paths shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"))
      tree3_2.paths shouldBe List(List("a"), List("a", "b"), List("a", "c"))
      tree4_1.paths shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"), List("a", "b", "c", "d"))
      tree4_2.paths shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"), List("a", "d"))
      tree4_3.paths shouldBe List(List("a"), List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.paths shouldBe List(
        List("a"),
        List("a", "b"),
        List("a", "b", "c"),
        List("a", "d"),
        List("a", "d", "e"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.paths shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "b", "c", "d"),
          List("a", "e"),
          List("a", "e", "f"),
          List("a", "e", "f", "g"),
          List("a", "e", "h"),
          List("a", "e", "h", "i")
        )
      tree13.paths shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "b", "c", "d"),
          List("a", "b", "e"),
          List("a", "b", "e", "f"),
          List("a", "b", "e", "g"),
          List("a", "b", "h"),
          List("a", "i"),
          List("a", "j"),
          List("a", "j", "k"),
          List("a", "j", "k", "l"),
          List("a", "j", "m")
        )
    }

    "iterate all branches" in {
      tree0.branches shouldBe Nil
      tree1.branches shouldBe List(List("a"))
      tree2.branches shouldBe List(List("a", "b"))
      tree3_1.branches shouldBe List(List("a", "b", "c"))
      tree3_2.branches shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branches shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branches shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branches shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branches shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      tree9.branches shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree13.branches shouldBe
        List(
          List("a", "b", "c", "d"),
          List("a", "b", "e", "f"),
          List("a", "b", "e", "g"),
          List("a", "b", "h"),
          List("a", "i"),
          List("a", "j", "k", "l"),
          List("a", "j", "m")
        )
    }

    "count all branches" in {
      tree0.countBranches(all) shouldBe 0
      tree1.countBranches(all) shouldBe 1
      tree2.countBranches(all) shouldBe 1
      tree3_1.countBranches(all) shouldBe 1
      tree3_2.countBranches(all) shouldBe 2
      tree4_1.countBranches(all) shouldBe 1
      tree4_2.countBranches(all) shouldBe 2
      tree4_3.countBranches(all) shouldBe 3
      tree7.countBranches(all) shouldBe 3
      tree9.countBranches(all) shouldBe 3

      tree0.countBranches(_.size > 2) shouldBe 0
      tree1.countBranches(_.size > 2) shouldBe 0
      tree2.countBranches(_.size > 2) shouldBe 0
      tree3_1.countBranches(_.size > 2) shouldBe 1
      tree3_2.countBranches(_.size > 2) shouldBe 0
      tree4_1.countBranches(_.size > 2) shouldBe 1
      tree4_2.countBranches(_.size > 2) shouldBe 1
      tree4_3.countBranches(_.size > 2) shouldBe 0
      tree7.countBranches(_.size > 2) shouldBe 2
      tree9.countBranches(_.size > 2) shouldBe 3

      tree0.countBranches(_.size <= 2) shouldBe 0
      tree1.countBranches(_.size <= 2) shouldBe 1
      tree2.countBranches(_.size <= 2) shouldBe 1
      tree3_1.countBranches(_.size <= 2) shouldBe 0
      tree3_2.countBranches(_.size <= 2) shouldBe 2
      tree4_1.countBranches(_.size <= 2) shouldBe 0
      tree4_2.countBranches(_.size <= 2) shouldBe 1
      tree4_3.countBranches(_.size <= 2) shouldBe 3
      tree7.countBranches(_.size <= 2) shouldBe 1
      tree9.countBranches(_.size <= 2) shouldBe 0
    }

    "iterate over paths with filter" in {
      tree0.pathsWithFilter(oddSize) shouldBe Nil
      tree1.pathsWithFilter(oddSize) shouldBe List(List("a"))
      tree2.pathsWithFilter(oddSize) shouldBe List(List("a"))
      tree3_1.pathsWithFilter(evenSize) shouldBe List(List("a", "b"))
      tree3_1.pathsWithFilter(oddSize) shouldBe List(List("a"), List("a", "b", "c"))
      tree3_2.pathsWithFilter(evenSize) shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.pathsWithFilter(evenSize) shouldBe List(List("a", "b"), List("a", "b", "c", "d"))
      tree4_1.pathsWithFilter(oddSize) shouldBe List(List("a"), List("a", "b", "c"))
      tree4_2.pathsWithFilter(evenSize) shouldBe List(List("a", "b"), List("a", "d"))
      tree4_2.pathsWithFilter(oddSize) shouldBe List(List("a"), List("a", "b", "c"))
      tree4_3.pathsWithFilter(evenSize) shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.pathsWithFilter(evenSize) shouldBe
        List(List("a", "b"), List("a", "d"), List("a", "d", "e", "f"), List("a", "g"))
      tree7.pathsWithFilter(oddSize) shouldBe List(List("a"), List("a", "b", "c"), List("a", "d", "e"))
      tree9.pathsWithFilter(oddSize) shouldBe
        List(List("a"), List("a", "b", "c"), List("a", "e", "f"), List("a", "e", "h"))
      tree9.pathsWithFilter(evenSize) shouldBe
        List(
          List("a", "b"),
          List("a", "b", "c", "d"),
          List("a", "e"),
          List("a", "e", "f", "g"),
          List("a", "e", "h", "i")
        )
    }

    "iterate over branches with filter" in {
      tree0.branchesWithFilter(all) shouldBe Nil
      tree1.branchesWithFilter(all) shouldBe List(List("a"))
      tree2.branchesWithFilter(all) shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all) shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(all) shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchesWithFilter(all) shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchesWithFilter(all) shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchesWithFilter(all) shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branchesWithFilter(all) shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchesWithFilter(all) shouldBe List(
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
      tree2.branchesWithFilter(_.size > 1) shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(_.last == "c") shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(_.last == "c") shouldBe List(List("a", "c"))
      tree4_2.branchesWithFilter(_.last == "d") shouldBe List(List("a", "d"))
      tree4_2.branchesWithFilter(_.size > 2) shouldBe List(List("a", "b", "c"))
      tree7.branchesWithFilter(_.size > 3) shouldBe List(List("a", "d", "e", "f"))
      tree9.branchesWithFilter(_.size > 3) shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchesWithFilter(_.toList.contains("e")) shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchesWithFilter(_.toList.contains("h")) shouldBe List(List("a", "e", "h", "i"))
      tree9.branchesWithFilter(_.size < 3) shouldBe Nil
    }

    "iterate over branches with filter and depth limit" in {
      tree0.branchesWithFilter(all, 0) shouldBe Nil
      tree0.branchesWithFilter(all, 1) shouldBe Nil
      tree1.branchesWithFilter(all, 0) shouldBe Nil
      tree1.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree1.branchesWithFilter(all, 2) shouldBe List(List("a"))
      tree2.branchesWithFilter(all, 0) shouldBe Nil
      tree2.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree2.branchesWithFilter(all, 2) shouldBe List(List("a", "b"))
      tree2.branchesWithFilter(all, 3) shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all, 0) shouldBe Nil
      tree3_1.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree3_1.branchesWithFilter(all, 2) shouldBe List(List("a", "b"))
      tree3_1.branchesWithFilter(all, 3) shouldBe List(List("a", "b", "c"))
      tree3_1.branchesWithFilter(all, 4) shouldBe List(List("a", "b", "c"))
      tree3_2.branchesWithFilter(all, 0) shouldBe Nil
      tree3_2.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree3_2.branchesWithFilter(all, 2) shouldBe List(List("a", "b"), List("a", "c"))
      tree3_2.branchesWithFilter(all, 3) shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchesWithFilter(all, 0) shouldBe Nil
      tree4_1.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree4_1.branchesWithFilter(all, 2) shouldBe List(List("a", "b"))
      tree4_1.branchesWithFilter(all, 3) shouldBe List(List("a", "b", "c"))
      tree4_1.branchesWithFilter(all, 4) shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchesWithFilter(all, 0) shouldBe Nil
      tree4_2.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree4_2.branchesWithFilter(all, 2) shouldBe List(List("a", "b"), List("a", "d"))
      tree4_2.branchesWithFilter(all, 3) shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_2.branchesWithFilter(all, 4) shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchesWithFilter(all, 0) shouldBe Nil
      tree4_3.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree4_3.branchesWithFilter(all, 2) shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      tree4_3.branchesWithFilter(all, 3) shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      tree7.branchesWithFilter(all, 0) shouldBe Nil
      tree7.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree7.branchesWithFilter(all, 2) shouldBe List(
        List("a", "b"),
        List("a", "d"),
        List("a", "g")
      )
      tree7.branchesWithFilter(all, 3) shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e"),
        List("a", "g")
      )
      tree7.branchesWithFilter(all, 4) shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchesWithFilter(all, 0) shouldBe Nil
      tree9.branchesWithFilter(all, 1) shouldBe List(List("a"))
      tree9.branchesWithFilter(all, 2) shouldBe List(List("a", "b"), List("a", "e"))
      tree9.branchesWithFilter(all, 3) shouldBe List(
        List("a", "b", "c"),
        List("a", "e", "f"),
        List("a", "e", "h")
      )
      tree9.branchesWithFilter(all, 4) shouldBe List(
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

    "iterate over paths with limit" in {
      tree0.pathsWithFilter(all, 3) shouldBe Nil
      tree1.pathsWithFilter(all, 3) shouldBe List(List("a"))
      tree2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"))
      tree3_1.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"))
      tree3_2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "c"))
      tree4_1
        .pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"))
      tree4_2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"), List("a", "d"))
      tree4_3.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.pathsWithFilter(all, 3) shouldBe List(
        List("a"),
        List("a", "b"),
        List("a", "b", "c"),
        List("a", "d"),
        List("a", "d", "e"),
        List("a", "g")
      )
      tree9.pathsWithFilter(all, 3) shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "e"),
          List("a", "e", "f"),
          List("a", "e", "h")
        )
      tree13.pathsWithFilter(all, 3) shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "b", "e"),
          List("a", "b", "h"),
          List("a", "i"),
          List("a", "j"),
          List("a", "j", "k"),
          List("a", "j", "m")
        )
    }

    "iterate over paths with limit" in {
      tree0.pathsWithFilter(all, 3) shouldBe Nil
      tree1.pathsWithFilter(all, 3) shouldBe List(List("a"))
      tree2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"))
      tree3_1.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"))
      tree3_2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "c"))
      tree4_1
        .pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"))
      tree4_2.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "b", "c"), List("a", "d"))
      tree4_3.pathsWithFilter(all, 3) shouldBe List(List("a"), List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.pathsWithFilter(all, 3) shouldBe List(
        List("a"),
        List("a", "b"),
        List("a", "b", "c"),
        List("a", "d"),
        List("a", "d", "e"),
        List("a", "g")
      )
      tree9.pathsWithFilter(all, 3) shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "e"),
          List("a", "e", "f"),
          List("a", "e", "h")
        )
      tree13.pathsWithFilter(all, 3) shouldBe
        List(
          List("a"),
          List("a", "b"),
          List("a", "b", "c"),
          List("a", "b", "e"),
          List("a", "b", "h"),
          List("a", "i"),
          List("a", "j"),
          List("a", "j", "k"),
          List("a", "j", "m")
        )
    }

  }

}
