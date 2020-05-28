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

import com.github.arturopala.tree.TreeFormat._

class TreeCreationSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "create an empty Tree" in {
      tree0.size shouldBe 0
      tree0.width shouldBe 0
      tree0.height shouldBe 0
      tree0.isLeaf shouldBe false
      tree0.childrenValues shouldBe Nil
      tree0.countBranches(_.nonEmpty) shouldBe 0
      tree0.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree0, "\n") shouldBe ""
      tree0.map(_ + 1) shouldBe Tree.empty
    }

    "create a single node Tree" in {
      tree1 shouldBe Tree("a")
      tree1.size shouldBe 1
      tree1.width shouldBe 1
      tree1.height shouldBe 1
      tree1.isLeaf shouldBe true
      tree1.childrenValues shouldBe Nil
      tree1.countBranches(_.nonEmpty) shouldBe 1
      tree1.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree1, "\n") shouldBe "[a]"
      tree1.map(_ + 1) shouldBe Tree("a1")
    }

    "create a double node Tree" in {
      tree2.size shouldBe 2
      tree2.width shouldBe 1
      tree2.height shouldBe 2
      tree2.isLeaf shouldBe false
      tree2.childrenValues shouldBe List("b")
      tree2.countBranches(_.nonEmpty) shouldBe 1
      tree2.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree2, "\n") shouldBe "[a,b]"
      tree2.map(_ + 1) shouldBe Tree("a1", Tree("b1"))
    }

    "create a three nodes Tree" in {
      tree3_1.size shouldBe 3
      tree3_1.width shouldBe 1
      tree3_1.height shouldBe 3
      tree3_1.isLeaf shouldBe false
      tree3_1.childrenValues shouldBe List("b")
      tree3_1.countBranches(_.nonEmpty) shouldBe 1
      val arrays1 = showAsArrays(tree3_1, "\n")
      arrays1 shouldBe "[a,b,c]"
      val newTree3_1 = tree3_1.map(_ + 1)
      newTree3_1 shouldBe Tree("a1", Tree("b1", Tree("c1")))

      tree3_2.size shouldBe 3
      tree3_2.width shouldBe 2
      tree3_2.height shouldBe 2
      tree3_2.isLeaf shouldBe false
      tree3_2.childrenValues shouldBe List("b", "c")
      tree3_2.countBranches(_.nonEmpty) shouldBe 2
      val arrays2 = showAsArrays(tree3_2, "\n")
      arrays2 shouldBe
        """[a,b]
          |[a,c]""".stripMargin
      val newTree2 = tree3_2.map(_ + 1)
      newTree2 shouldBe Tree("a1", Tree("b1"), Tree("c1"))
    }

    "create a four nodes Tree" in {
      tree4_1.size shouldBe 4
      tree4_1.width shouldBe 1
      tree4_1.height shouldBe 4
      tree4_1.isLeaf shouldBe false
      tree4_1.countBranches(_.nonEmpty) shouldBe 1
      tree4_1.childrenValues shouldBe List("b")
      showAsArrays(tree4_1, "\n") shouldBe "[a,b,c,d]"
      tree4_1.map(_ + 1) shouldBe Tree("a1", Tree("b1", Tree("c1", Tree("d1"))))

      tree4_2.size shouldBe 4
      tree4_2.width shouldBe 2
      tree4_2.height shouldBe 3
      tree4_2.isLeaf shouldBe false
      tree4_2.childrenValues shouldBe List("b", "d")
      tree4_2.countBranches(_.nonEmpty) shouldBe 2
      showAsArrays(tree4_2, "\n") shouldBe
        """[a,b,c]
          |[a,d]""".stripMargin
      tree4_2.map(_ + 1) shouldBe Tree("a1", Tree("b1", Tree("c1")), Tree("d1"))

      tree4_3.size shouldBe 4
      tree4_3.width shouldBe 3
      tree4_3.height shouldBe 2
      tree4_3.isLeaf shouldBe false
      tree4_3.childrenValues shouldBe List("b", "c", "d")
      tree4_3.countBranches(_.nonEmpty) shouldBe 3
      tree4_3.countBranches(_.toSeq.contains("c")) shouldBe 1
      showAsArrays(tree4_3, "\n") shouldBe
        """[a,b]
          |[a,c]
          |[a,d]""".stripMargin
      tree4_3.map(_ + 1) shouldBe Tree("a1", Tree("b1"), Tree("c1"), Tree("d1"))
    }

    "create a multi-branch Tree" in {
      tree9.size shouldBe 9
      tree9.width shouldBe 3
      tree9.height shouldBe 4
      tree9.isLeaf shouldBe false
      tree9.countBranches(_.nonEmpty) shouldBe 3
      tree9.countBranches(_.toSeq.contains("e")) shouldBe 2
      tree9.childrenValues.toList shouldBe List("b", "e")
      tree9.children.toList shouldBe List(
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      showAsArrays(tree9, "\n") shouldBe
        """[a,b,c,d]
          |[a,e,f,g]
          |[a,e,h,i]""".stripMargin

      tree7.childrenCount shouldBe 3
      tree7.childrenValues.toList shouldBe List("b", "d", "g")
      tree7.children.toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
    }

  }

}
