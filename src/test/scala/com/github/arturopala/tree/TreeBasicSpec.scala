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

class TreeBasicSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "be equal to the other tree if both have same structure and content" in {
      tree0 shouldBe Tree()
      tree0.hashCode() shouldBe Tree().hashCode()
      tree0 shouldBe Tree()
      //tree0 shouldBe Tree().deflated
      //tree0.hashCode() shouldBe Tree().deflated.hashCode() // Fails in Dotty

      tree1 shouldBe Tree("a")
      tree1.hashCode() shouldBe Tree("a").hashCode()
      tree1 shouldBe Tree("a")
      tree1 shouldBe Tree("a").deflated
      tree1.hashCode() shouldBe Tree("a").deflated.hashCode()
      TreeBuilder.fromArraysHead(Array(0), Array("a")) shouldBe Tree("a")
      TreeBuilder.fromArraysHead(Array(0), Array("a")).hashCode() shouldBe Tree("a")
        .hashCode()

      tree2 shouldBe Tree("a", Tree("b"))
      tree2.hashCode() shouldBe Tree("a", Tree("b")).hashCode()
      tree2 shouldBe Tree("a", Tree("b"))
      tree2 shouldBe Tree("a", Tree("b")).deflated
      tree2.hashCode() shouldBe Tree("a", Tree("b")).deflated.hashCode()
      TreeBuilder.fromArraysHead(Array(0, 1), Array("b", "a")) shouldBe Tree("a", Tree("b"))
      TreeBuilder.fromArraysHead(Array(0, 1), Array("b", "a")).hashCode() shouldBe Tree("a", Tree("b")).hashCode()

      tree3_1 shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.hashCode() shouldBe Tree("a", Tree("b", Tree("c"))).hashCode()
      tree3_1 shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1 shouldBe Tree("a", Tree("b", Tree("c"))).deflated
      tree3_1.hashCode() shouldBe Tree("a", Tree("b", Tree("c"))).deflated.hashCode()
      TreeBuilder.fromArraysHead(Array(0, 1, 1), Array("c", "b", "a")) shouldBe Tree("a", Tree("b", Tree("c")))
      TreeBuilder.fromArraysHead(Array(0, 1, 1), Array("c", "b", "a")).hashCode() shouldBe Tree(
        "a",
        Tree("b", Tree("c"))
      ).hashCode()

      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2.hashCode() shouldBe Tree("a", Tree("b"), Tree("c")).hashCode()
      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c")).deflated
      tree3_2.hashCode() shouldBe Tree("a", Tree("b"), Tree("c")).deflated.hashCode()
      TreeBuilder.fromArraysHead(Array(0, 0, 2), Array("c", "b", "a")) shouldBe Tree("a", Tree("b"), Tree("c"))
      TreeBuilder.fromArraysHead(Array(0, 0, 2), Array("c", "b", "a")).hashCode() shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c")
      ).hashCode()
    }

    "be not equal to the tree with different structure" in {
      tree3_1 should not be tree3_2
      tree4_1 should not be tree4_2
      tree4_2 should not be tree4_3
      tree4_1 should not be tree4_3
    }

    "hashcode should differ for different trees" in {
      allTrees.map(_.hashCode()).toSet.size shouldBe 10
      Tree(0).hashCode() should not be Tree(1).hashCode()
      Tree(0, Tree(1)).hashCode() should not be Tree(1, Tree(0)).hashCode()
    }

    "be not equal to the tree with different content" in {
      Tree(0) should not be Tree(1)
      tree1 should not be Tree("A")
      tree1 should not be Tree("b")
      Tree(0, Tree(1)) should not be Tree(1, Tree(0))
      tree2 should not be Tree("b", Tree("a"))
      tree2 should not be Tree("ab")
      tree2 should not be Tree("ba")
      Tree("ab") should not be Tree("ba")
    }

  }

}
