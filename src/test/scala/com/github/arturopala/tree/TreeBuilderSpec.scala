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

import com.github.arturopala.tree.TreeBuilder._
import com.github.arturopala.tree.TreeFormat.showAsGraph

class TreeBuilderSpec extends AnyWordSpecCompat {

  "TreeBuilder" should {
    "create a new tree from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (0, "b"), (0, "c"), (3, "d"))

      val trees = fromPairsIterable(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.width shouldBe 3
      trees.head shouldBe Tree("d", Tree("c"), Tree("b"), Tree("a"))
      showAsGraph(trees.head, "\n") shouldBe
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

      val trees = fromTreePairsList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.width shouldBe 3
      trees.head shouldBe Tree("d", Tree("c"), Tree("b"), Tree("a"))
      showAsGraph(trees.head, "\n") shouldBe
        """d > c
          |d > b
          |d > a""".stripMargin
    }

    "create a new tree from the list of multi-node trees (1)" in {
      val list: List[(Int, Tree[String])] =
        List((0, Tree("a", Tree("A"))), (0, Tree("b", Tree("B"))), (0, Tree("c", Tree("C"))), (3, Tree("d", Tree("D"))))

      val trees = fromTreePairsList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 8
      trees.head.width shouldBe 4
      trees.head shouldBe Tree("d", Tree("c", Tree("C")), Tree("b", Tree("B")), Tree("a", Tree("A")), Tree("D"))
      showAsGraph(trees.head, "\n") shouldBe
        """d > c > C
          |d > b > B
          |d > a > A
          |d > D""".stripMargin
    }

    "create a new tree from the list of multi-node trees (2)" in {
      val list: List[(Int, Tree[String])] =
        List((0, Tree("a", Tree("A"))), (1, Tree("b", Tree("B"))), (1, Tree("c", Tree("C"))), (1, Tree("d", Tree("D"))))

      val trees = fromTreePairsList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 8
      trees.head.width shouldBe 4
      trees.head shouldBe Tree("d", Tree("c", Tree("b", Tree("a", Tree("A")), Tree("B")), Tree("C")), Tree("D"))
      showAsGraph(trees.head, "\n") shouldBe
        """d > c > b > a > A
          |d > c > b > B
          |d > c > C
          |d > D""".stripMargin
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

    "create a new single-branch tree from a list of values" in {
      //fromValueList(List()) shouldBe Tree.empty
      linearTreeFromList(List("a")) shouldBe Tree("a")
      linearTreeFromList(List("a", "b", "c")) shouldBe Tree("a", Tree("b", Tree("c")))
      linearTreeFromList(List("a", "a", "a")) shouldBe Tree("a", Tree("a", Tree("a")))
    }

    "create a new main-branch tree from a list of trees" in {
      //fromTreeList(List()) shouldBe Tree.empty
      fromTreeList(List(Tree("a"))) shouldBe Tree("a")
      fromTreeList(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      fromTreeList(List(Tree("c"), Tree("b"), Tree("a"))) shouldBe Tree("c", Tree("b", Tree("a")))
      fromTreeList(List(Tree("a"), Tree("a"), Tree("a"))) shouldBe Tree("a", Tree("a", Tree("a")))
      fromTreeList(List(Tree("a", Tree("b")), Tree("a", Tree("c")), Tree("a", Tree("d"), Tree("e")))) shouldBe Tree(
        "a",
        Tree("a", Tree("a", Tree("d"), Tree("e")), Tree("c")),
        Tree("b")
      )
    }

    "create a tree from tree split lists and a tree" in {
      fromTreeSplitAndChild(Tree.empty, List()) shouldBe Tree.empty
      fromTreeSplitAndChild(Tree("a"), List()) shouldBe Tree("a")
      fromTreeSplitAndChild(Tree.empty, List((Nil, "a", Nil))) shouldBe Tree("a")
      fromTreeSplitAndChild(Tree("a"), List((Nil, "b", Nil), (Nil, "c", Nil), (Nil, "d", Nil))) shouldBe Tree(
        "d",
        Tree("c", Tree("b", Tree("a")))
      )
      fromTreeSplitAndChild(
        Tree("a"),
        List((List(Tree("e")), "b", Nil), (List(Tree("f")), "c", Nil), (List(Tree("g")), "d", Nil))
      ) shouldBe Tree("d", Tree("g"), Tree("c", Tree("f"), Tree("b", Tree("e"), Tree("a"))))
      fromTreeSplitAndChild(
        Tree("a"),
        List(
          (List(Tree("e")), "b", List(Tree("h"))),
          (List(Tree("f")), "c", List(Tree("i"))),
          (List(Tree("g")), "d", List(Tree("j")))
        )
      ) shouldBe Tree(
        "d",
        Tree("g"),
        Tree("c", Tree("f"), Tree("b", Tree("e"), Tree("a"), Tree("h")), Tree("i")),
        Tree("j")
      )
    }
  }

}
