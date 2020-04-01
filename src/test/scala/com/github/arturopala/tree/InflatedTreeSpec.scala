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

import com.github.arturopala.tree.Tree.Show.showAsGraph

class InflatedTreeSpec extends TreeSpec {

  override def name = "InflatedTree"

  val tree0: Tree[String] = TestTrees.tree0
  val tree1: Tree[String] = TestTrees.tree1
  val tree2: Tree[String] = TestTrees.tree2
  val tree3_1: Tree[String] = TestTrees.tree3_1
  val tree3_2: Tree[String] = TestTrees.tree3_2
  val tree4_1: Tree[String] = TestTrees.tree4_1
  val tree4_2: Tree[String] = TestTrees.tree4_2
  val tree4_3: Tree[String] = TestTrees.tree4_3
  val tree7: Tree[String] = TestTrees.tree7
  val tree9: Tree[String] = TestTrees.tree9

  "Tree.Builder" should {
    "create a new tree from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (0, "b"), (0, "c"), (3, "d"))

      val trees = Tree.Builder.fromPairsIterable(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.width shouldBe 3
      trees.head shouldBe Tree("d", Tree("c"), Tree("b"), Tree("a"))
      showAsGraph(trees.head) shouldBe
        """d > c
          |d > b
          |d > a""".stripMargin
    }

    "create a new trees from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (1, "b"), (0, "c"), (1, "d"))

      val trees = Tree.Builder.fromPairsIterable(list)

      trees.size shouldBe 2
      trees(0) shouldBe Tree("d", Tree("c"))
      trees(1) shouldBe Tree("b", Tree("a"))
    }

    "create a new tree from the list of single-node trees" in {
      val list: List[(Int, Tree[String])] = List((0, Tree("a")), (0, Tree("b")), (0, Tree("c")), (3, Tree("d")))

      val trees = Tree.Builder.fromTreeList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.width shouldBe 3
      trees.head shouldBe Tree("d", Tree("c"), Tree("b"), Tree("a"))
      showAsGraph(trees.head) shouldBe
        """d > c
          |d > b
          |d > a""".stripMargin
    }

    "create a new tree from the list of multi-node trees (1)" in {
      val list: List[(Int, Tree[String])] =
        List((0, Tree("a", Tree("A"))), (0, Tree("b", Tree("B"))), (0, Tree("c", Tree("C"))), (3, Tree("d", Tree("D"))))

      val trees = Tree.Builder.fromTreeList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 8
      trees.head.width shouldBe 4
      trees.head shouldBe Tree("d", Tree("c", Tree("C")), Tree("b", Tree("B")), Tree("a", Tree("A")), Tree("D"))
      showAsGraph(trees.head) shouldBe
        """d > c > C
          |d > b > B
          |d > a > A
          |d > D""".stripMargin
    }

    "create a new tree from the list of multi-node trees (2)" in {
      val list: List[(Int, Tree[String])] =
        List((0, Tree("a", Tree("A"))), (1, Tree("b", Tree("B"))), (1, Tree("c", Tree("C"))), (1, Tree("d", Tree("D"))))

      val trees = Tree.Builder.fromTreeList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 8
      trees.head.width shouldBe 4
      trees.head shouldBe Tree("d", Tree("c", Tree("b", Tree("a", Tree("A")), Tree("B")), Tree("C")), Tree("D"))
      showAsGraph(trees.head) shouldBe
        """d > c > b > a > A
          |d > c > b > B
          |d > c > C
          |d > D""".stripMargin
    }

    "create a new tree from the list of multi-node trees using replace strategy" in {
      val list: List[(Int, Tree[String])] =
        List((0, Tree("a", Tree("A"))), (0, Tree("b", Tree("B"))), (0, Tree("c", Tree("C"))), (3, Tree("d", Tree("D"))))

      val trees = Tree.Builder.fromTreeList(list, strategy = Tree.FlatMapStrategy.Replace)

      trees.size shouldBe 1
      trees.head.size shouldBe 2
      trees.head.width shouldBe 1
      trees.head shouldBe Tree("d", Tree("D"))
      showAsGraph(trees.head) shouldBe "d > D"
    }

    "be equal to the other tree if both have same structure and content" in {
      tree0 shouldBe Tree()
      tree0.hashCode() shouldBe Tree().hashCode()
      tree0 shouldBe Tree()
      tree0 shouldBe Tree().deflate
      tree0.hashCode() shouldBe Tree().deflate.hashCode()

      tree1 shouldBe Tree("a")
      tree1.hashCode() shouldBe Tree("a").hashCode()
      tree1 shouldBe Tree("a")
      tree1 shouldBe Tree("a").deflate
      tree1.hashCode() shouldBe Tree("a").deflate.hashCode()
      Tree.Builder.fromArraysHead(Array(0), Array("a")) shouldBe Tree("a")
      Tree.Builder.fromArraysHead(Array(0), Array("a")).hashCode() shouldBe Tree("a")
        .hashCode()

      tree2 shouldBe Tree("a", Tree("b"))
      tree2.hashCode() shouldBe Tree("a", Tree("b")).hashCode()
      tree2 shouldBe Tree("a", Tree("b"))
      tree2 shouldBe Tree("a", Tree("b")).deflate
      tree2.hashCode() shouldBe Tree("a", Tree("b")).deflate.hashCode()
      Tree.Builder.fromArraysHead(Array(0, 1), Array("b", "a")) shouldBe Tree("a", Tree("b"))
      Tree.Builder.fromArraysHead(Array(0, 1), Array("b", "a")).hashCode() shouldBe Tree("a", Tree("b")).hashCode()

      tree3_1 shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.hashCode() shouldBe Tree("a", Tree("b", Tree("c"))).hashCode()
      tree3_1 shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1 shouldBe Tree("a", Tree("b", Tree("c"))).deflate
      tree3_1.hashCode() shouldBe Tree("a", Tree("b", Tree("c"))).deflate.hashCode()
      Tree.Builder.fromArraysHead(Array(0, 1, 1), Array("c", "b", "a")) shouldBe Tree("a", Tree("b", Tree("c")))
      Tree.Builder.fromArraysHead(Array(0, 1, 1), Array("c", "b", "a")).hashCode() shouldBe Tree(
        "a",
        Tree("b", Tree("c"))
      ).hashCode()

      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2.hashCode() shouldBe Tree("a", Tree("b"), Tree("c")).hashCode()
      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2 shouldBe Tree("a", Tree("b"), Tree("c")).deflate
      tree3_2.hashCode() shouldBe Tree("a", Tree("b"), Tree("c")).deflate.hashCode()
      Tree.Builder.fromArraysHead(Array(0, 0, 2), Array("c", "b", "a")) shouldBe Tree("a", Tree("b"), Tree("c"))
      Tree.Builder.fromArraysHead(Array(0, 0, 2), Array("c", "b", "a")).hashCode() shouldBe Tree(
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

    "be not equal to the tree with different content" in {
      Tree(0) should not be Tree(1)
      Tree("a") should not be Tree("A")
      Tree("a") should not be Tree("b")
      Tree(0, Tree(1)) should not be Tree(1, Tree(0))
      Tree("a", Tree("b")) should not be Tree("b", Tree("a"))
      Tree("a", Tree("b")) should not be Tree("ab")
      Tree("a", Tree("b")) should not be Tree("ba")
      Tree("ab") should not be Tree("ba")
    }

    "hashcode should differ for different trees" in {
      Seq(tree0, tree1, tree2, tree3_1, tree3_2, tree4_1, tree4_2, tree4_3, tree7, tree9)
        .map(_.hashCode())
        .toSet
        .size shouldBe 10
      Tree(0).hashCode() should not be Tree(1).hashCode()
      Tree(0, Tree(1)).hashCode() should not be Tree(1, Tree(0)).hashCode()
    }
  }

}
