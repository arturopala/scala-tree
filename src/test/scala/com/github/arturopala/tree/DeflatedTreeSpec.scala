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

import com.github.arturopala.tree.Tree.Show._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeflatedTreeSpec extends AnyWordSpec with Matchers {

  val tree0 = Tree[String]().deflate
  val tree1 = Tree("a").deflate
  val tree2 = Tree("a", Tree("b")).deflate
  val tree3_1 = Tree("a", Tree("b", Tree("c"))).deflate
  val tree3_2 = Tree("a", Tree("b"), Tree("c")).deflate
  val tree4_1 = Tree("a", Tree("b", Tree("c", Tree("d")))).deflate
  val tree4_2 = Tree("a", Tree("b", Tree("c")), Tree("d")).deflate
  val tree4_3 = Tree("a", Tree("b"), Tree("c"), Tree("d")).deflate
  val tree7 = Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")).deflate
  val tree9 = Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))).deflate

  "Tree.ArrayTree" should {
    "create an empty Tree" in {
      tree0.size shouldBe 0
      tree0.width shouldBe 0
      tree0.height shouldBe 0
      tree0.isLeaf shouldBe false
      tree0.children shouldBe Nil
      tree0.countBranches(_.nonEmpty) shouldBe 0
      tree0.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree0) shouldBe ""
      tree0.map(_ + 1) shouldBe Tree.empty
    }

    "create a single node Tree" in {
      tree1 shouldBe Tree("a")
      tree1.size shouldBe 1
      tree1.width shouldBe 1
      tree1.height shouldBe 1
      tree1.isLeaf shouldBe true
      tree1.children shouldBe Nil
      tree1.countBranches(_.nonEmpty) shouldBe 1
      tree1.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree1) shouldBe "[a]"
      tree1.map(_ + 1) shouldBe Tree("a1")
    }

    "create a double node Tree" in {
      tree2.size shouldBe 2
      tree2.width shouldBe 1
      tree2.height shouldBe 2
      tree2.isLeaf shouldBe false
      tree2.children shouldBe List("b")
      tree2.countBranches(_.nonEmpty) shouldBe 1
      showAsArrays(tree2) shouldBe "[a,b]"
      tree2.map(_ + 1) shouldBe Tree("a1", Tree("b1"))
    }

    "create a three nodes Tree" in {
      tree3_1.size shouldBe 3
      tree3_1.width shouldBe 1
      tree3_1.height shouldBe 3
      tree3_1.isLeaf shouldBe false
      tree3_1.children shouldBe List("b")
      tree3_1.countBranches(_.nonEmpty) shouldBe 1
      val arrays1 = showAsArrays(tree3_1)
      arrays1 shouldBe "[a,b,c]"
      val newTree3_1 = tree3_1.map(_ + 1)
      newTree3_1 shouldBe Tree("a1", Tree("b1", Tree("c1")))

      tree3_2.size shouldBe 3
      tree3_2.width shouldBe 2
      tree3_2.height shouldBe 2
      tree3_2.isLeaf shouldBe false
      tree3_2.children shouldBe List("b", "c")
      tree3_2.countBranches(_.nonEmpty) shouldBe 2
      val arrays2 = showAsArrays(tree3_2)
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
      tree4_1.children shouldBe List("b")
      showAsArrays(tree4_1) shouldBe "[a,b,c,d]"
      tree4_1.map(_ + 1) shouldBe Tree("a1", Tree("b1", Tree("c1", Tree("d1"))))

      tree4_2.size shouldBe 4
      tree4_2.width shouldBe 2
      tree4_2.height shouldBe 3
      tree4_2.isLeaf shouldBe false
      tree4_2.children shouldBe List("b", "d")
      tree4_2.countBranches(_.nonEmpty) shouldBe 2
      showAsArrays(tree4_2) shouldBe
        """[a,b,c]
          |[a,d]""".stripMargin
      tree4_2.map(_ + 1) shouldBe Tree("a1", Tree("b1", Tree("c1")), Tree("d1"))

      tree4_3.size shouldBe 4
      tree4_3.width shouldBe 3
      tree4_3.height shouldBe 2
      tree4_3.isLeaf shouldBe false
      tree4_3.children shouldBe List("b", "c", "d")
      tree4_3.countBranches(_.nonEmpty) shouldBe 3
      tree4_3.countBranches(_.contains("c")) shouldBe 1
      showAsArrays(tree4_3) shouldBe
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
      tree9.countBranches(_.contains("e")) shouldBe 2
      showAsArrays(tree9) shouldBe
        """[a,b,c,d]
          |[a,e,f,g]
          |[a,e,h,i]""".stripMargin
    }

    "check if contains branch" in {
      tree0.containsBranch(List("a", "b")) shouldBe false
      tree9.containsBranch(List("a", "b", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "h", "i")) shouldBe true
      tree9.containsBranch(List("a", "e", "c")) shouldBe false
    }

    "check if contains path" in {
      tree0.containsPath(List("a", "b")) shouldBe false
      tree9.containsPath(List("a", "b", "c")) shouldBe true
      tree9.containsPath(List("a", "e", "h", "i")) shouldBe true
      tree9.containsPath(List("a", "e", "c")) shouldBe false
    }

    "insert new node to an empty Tree" in {
      val tree1 = tree0.insert("a")
      tree1 shouldBe Tree("a")
      tree1.size shouldBe 1
      tree1.width shouldBe 1
    }

    "return nodes in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).nodes shouldBe tree3_2.nodes
      Tree.inflate(tree7).nodes shouldBe tree7.nodes
      Tree.inflate(tree9).nodes shouldBe tree9.nodes
    }

    "return children in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).children shouldBe tree3_2.children
      Tree.inflate(tree7).children shouldBe tree7.children
      Tree.inflate(tree9).children shouldBe tree9.children
    }

    "return branches in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).branches shouldBe tree3_2.branches
      Tree.inflate(tree7).branches shouldBe tree7.branches
      Tree.inflate(tree9).branches shouldBe tree9.branches
    }
  }

}
