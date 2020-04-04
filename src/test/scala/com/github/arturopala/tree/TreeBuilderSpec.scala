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

import com.github.arturopala.tree.TreeBuilder.{fromArrays, fromPairsIterable, fromTreeList}
import com.github.arturopala.tree.Tree.Show.showAsGraph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeBuilderSpec extends AnyWordSpec with Matchers {

  "TreeBuilder" should {
    "create a new tree from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (0, "b"), (0, "c"), (3, "d"))

      val trees = fromPairsIterable(list)

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

      val trees = fromPairsIterable(list)

      trees.size shouldBe 2
      trees(0) shouldBe Tree("d", Tree("c"))
      trees(1) shouldBe Tree("b", Tree("a"))
    }

    "create a new tree from the list of single-node trees" in {
      val list: List[(Int, Tree[String])] = List((0, Tree("a")), (0, Tree("b")), (0, Tree("c")), (3, Tree("d")))

      val trees = fromTreeList(list)

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

      val trees = fromTreeList(list)

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

      val trees = fromTreeList(list)

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

      val trees = fromTreeList(list, strategy = TreeBuilder.FlatMapStrategy.Replace)

      trees.size shouldBe 1
      trees.head.size shouldBe 2
      trees.head.width shouldBe 1
      trees.head shouldBe Tree("d", Tree("D"))
      showAsGraph(trees.head) shouldBe "d > D"
    }

    "create a new tree from the pair of arrays" in {
      fromArrays(Array.empty[Int], Array.empty[String]) shouldBe List(Tree.empty)
      fromArrays(Array(0), Array("a")) shouldBe List(Tree("a"))
      fromArrays(Array(0, 1), Array("aa", "a")) shouldBe List(Tree("a", Tree("aa")))
      fromArrays(Array(0, 0), Array("aa", "a")) shouldBe List(Tree("a"), Tree("aa"))
      fromArrays(Array(0, 1), Array("a", "aa")) shouldBe List(Tree("aa", Tree("a")))
      fromArrays(Array(0, 1), Array("a", "aa")) shouldBe List(Tree("aa", Tree("a")))
      fromArrays(Array(0, 1, 1), Array("aaa", "aa", "a")) shouldBe List(Tree("a", Tree("aa", Tree("aaa"))))
      fromArrays(Array(0, 0, 2), Array("aaa", "aa", "a")) shouldBe List(Tree("a", Tree("aa"), Tree("aaa")))
      fromArrays(Array(0, 0, 1), Array("aaa", "aa", "a")) shouldBe List(Tree("a", Tree("aa")), Tree("aaa"))
      fromArrays(Array(0, 0, 0), Array("aaa", "aa", "a")) shouldBe List(Tree("a"), Tree("aa"), Tree("aaa"))
      fromArrays(Array(0, 1, 0), Array("aaa", "aa", "a")) shouldBe List(Tree("a"), Tree("aa", Tree("aaa")))
      fromArrays(Array(1, 0, 0), Array("aaa", "aa", "a")) shouldBe List(Tree("a"), Tree("aa"), Tree.empty)
    }
  }

}
