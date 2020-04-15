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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait TreeSpec extends AnyWordSpec with Matchers {

  def name: String

  val tree0: Tree[String]
  val tree1: Tree[String]
  val tree2: Tree[String]
  val tree3_1: Tree[String]
  val tree3_2: Tree[String]
  val tree4_1: Tree[String]
  val tree4_2: Tree[String]
  val tree4_3: Tree[String]
  val tree7: Tree[String]
  val tree9: Tree[String]
  val allTrees: Seq[Tree[String]]

  def all[T]: T => Boolean = _ => true
  def none[T]: T => Boolean = _ => false
  val even: String => Boolean = s => s.head.toInt % 2 == 0
  val odd: String => Boolean = s => s.head.toInt  % 2 != 0

  s"$name" should {

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
      showAsArrays(tree9, "\n") shouldBe
        """[a,b,c,d]
          |[a,e,f,g]
          |[a,e,h,i]""".stripMargin
    }

    "check if tree contains a branch" in {
      tree0.containsBranch(List()) shouldBe false
      tree0.containsBranch(List("a")) shouldBe false
      tree0.containsBranch(List("a", "b")) shouldBe false
      tree1.containsBranch(List("a")) shouldBe true
      tree1.containsBranch(List("b")) shouldBe false
      tree1.containsBranch(List("a", "b")) shouldBe false
      tree2.containsBranch(List("a")) shouldBe false
      tree2.containsBranch(List("a", "b")) shouldBe true
      tree2.containsBranch(List("a", "c")) shouldBe false
      tree2.containsBranch(List("a", "b", "c")) shouldBe false
      tree3_1.containsBranch(List("a", "b", "c")) shouldBe true
      tree3_1.containsBranch(List("a", "b")) shouldBe false
      tree3_1.containsBranch(List("a", "c")) shouldBe false
      tree3_1.containsBranch(List("a")) shouldBe false
      tree3_1.containsBranch(List("b")) shouldBe false
      tree3_1.containsBranch(List("b", "c")) shouldBe false
      tree3_2.containsBranch(List("a", "b")) shouldBe true
      tree3_2.containsBranch(List("a", "c")) shouldBe true
      tree3_2.containsBranch(List("a", "a")) shouldBe false
      tree3_2.containsBranch(List("a")) shouldBe false
      tree3_2.containsBranch(List("b")) shouldBe false
      tree3_2.containsBranch(List("c")) shouldBe false
      tree3_2.containsBranch(List("a", "b", "c")) shouldBe false
      tree3_2.containsBranch(List("a", "c", "b")) shouldBe false
      tree3_2.containsBranch(List("a", "c", "e")) shouldBe false
      tree4_1.containsBranch(List("a", "b", "c", "d")) shouldBe true
      tree4_1.containsBranch(List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.containsBranch(List("a", "b", "c")) shouldBe false
      tree4_1.containsBranch(List("a", "b")) shouldBe false
      tree4_1.containsBranch(List("a")) shouldBe false
      tree4_2.containsBranch(List("a", "b", "c")) shouldBe true
      tree4_2.containsBranch(List("a", "d")) shouldBe true
      tree4_2.containsBranch(List("a", "b", "c", "d")) shouldBe false
      tree4_2.containsBranch(List("a", "a", "a")) shouldBe false
      tree4_2.containsBranch(List("c", "b", "a")) shouldBe false
      tree4_2.containsBranch(List("d", "a")) shouldBe false
      tree9.containsBranch(List("a", "b", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "h", "i")) shouldBe true
      tree9.containsBranch(List("a", "e", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "f")) shouldBe false
      tree9.containsBranch(List("a", "e", "h")) shouldBe false
    }

    "check if tree contains a path" in {
      tree0.containsPath(List()) shouldBe false
      tree0.containsPath(List("a")) shouldBe false
      tree0.containsPath(List("a", "b")) shouldBe false
      tree1.containsPath(List("a")) shouldBe true
      tree1.containsPath(List("b")) shouldBe false
      tree1.containsPath(List("a", "b")) shouldBe false
      tree2.containsPath(List("a")) shouldBe true
      tree2.containsPath(List("a", "b")) shouldBe true
      tree2.containsPath(List("a", "c")) shouldBe false
      tree2.containsPath(List("a", "b", "c")) shouldBe false
      tree3_1.containsPath(List("a", "b", "c")) shouldBe true
      tree3_1.containsPath(List("a", "b")) shouldBe true
      tree3_1.containsPath(List("a", "c")) shouldBe false
      tree3_1.containsPath(List("a")) shouldBe true
      tree3_1.containsPath(List("b")) shouldBe false
      tree3_1.containsPath(List("b", "c")) shouldBe false
      tree3_2.containsPath(List("a", "b")) shouldBe true
      tree3_2.containsPath(List("a", "c")) shouldBe true
      tree3_2.containsPath(List("a", "a")) shouldBe false
      tree3_2.containsPath(List("a")) shouldBe true
      tree3_2.containsPath(List("b")) shouldBe false
      tree3_2.containsPath(List("c")) shouldBe false
      tree3_2.containsPath(List("a", "b", "c")) shouldBe false
      tree3_2.containsPath(List("a", "c", "b")) shouldBe false
      tree3_2.containsPath(List("a", "c", "e")) shouldBe false
      tree4_1.containsPath(List("a", "b", "c", "d")) shouldBe true
      tree4_1.containsPath(List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.containsPath(List("a", "b", "c")) shouldBe true
      tree4_1.containsPath(List("a", "b")) shouldBe true
      tree4_1.containsPath(List("a")) shouldBe true
      tree4_2.containsPath(List("a", "b", "c")) shouldBe true
      tree4_2.containsPath(List("a", "d")) shouldBe true
      tree4_2.containsPath(List("a", "b", "c", "d")) shouldBe false
      tree4_2.containsPath(List("a", "b")) shouldBe true
      tree4_2.containsPath(List("a", "a", "a")) shouldBe false
      tree4_2.containsPath(List("c", "b", "a")) shouldBe false
      tree4_2.containsPath(List("d", "a")) shouldBe false
      tree9.containsPath(List("a", "b", "c")) shouldBe true
      tree9.containsPath(List("a", "e", "h", "i")) shouldBe true
      tree9.containsPath(List("a", "e", "f")) shouldBe true
      tree9.containsPath(List("a", "e", "h")) shouldBe true
    }

    "insert new node to a tree" in {
      val result0 = tree0.insertValue("a")
      result0 shouldBe Tree("a")
      result0.size shouldBe 1
      result0.width shouldBe 1

      val result1 = tree1.insertValue("b")
      result1 shouldBe Tree("a", Tree("b"))
      result1.size shouldBe 2
      result1.width shouldBe 1

      tree2.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertValue("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertValue("b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
    }

    "insert new subtree to a tree" in {
      val result0 = tree0.insertTree(Tree("a"))
      result0 shouldBe Tree("a")
      result0.size shouldBe 1
      result0.width shouldBe 1

      val result1 = tree1.insertTree(Tree("b"))
      result1 shouldBe Tree("a", Tree("b"))
      result1.size shouldBe 2
      result1.width shouldBe 1

      tree2.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertTree(Tree("d")) shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertTree(Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))

      tree1.insertTree(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertTree(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")))
      tree1.insertTree(Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")))

      tree2.insertTree(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree2.insertTree(tree2) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree3_1.insertTree(tree3_1) shouldBe Tree("a", Tree("a", Tree("b", Tree("c"))), Tree("b", Tree("c")))
      tree3_2.insertTree(tree3_2) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("b"), Tree("c"))
      tree4_1.insertTree(tree4_1) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_2.insertTree(tree4_2) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d")
      )
      tree4_3.insertTree(tree4_3) shouldBe Tree(
        "a",
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree7.insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTree(tree7).insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      allTrees.foldLeft(tree0)((acc, tree) => tree.insertTree(acc)) shouldBe Tree(
        "a",
        Tree(
          "a",
          Tree(
            "a",
            Tree(
              "a",
              Tree(
                "a",
                Tree("a", Tree("a", Tree("a", Tree("a"), Tree("b")), Tree("b", Tree("c"))), Tree("b"), Tree("c")),
                Tree("b", Tree("c", Tree("d")))
              ),
              Tree("b", Tree("c")),
              Tree("d")
            ),
            Tree("b"),
            Tree("c"),
            Tree("d")
          ),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        ),
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )

      allTrees.foldLeft(tree0)((acc, tree) => acc.insertTree(tree)) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("a", Tree("b"), Tree("c")),
        Tree("a", Tree("b", Tree("c"))),
        Tree("a", Tree("b"))
      )
    }

    "insert new branch to a tree" in {
      tree0.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("b", "c", "d")) shouldBe Tree("a")
      tree2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree2
        .insertBranch(List("a", "b", "c", "d"))
        .insertBranch(List("a", "b", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f")), Tree("c", Tree("d")))
      )
      tree3_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertBranch(List("a", "b", "e", "f")) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c")))
      tree3_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c"))
      tree4_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree4_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("d"))
      tree4_3.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("c"),
        Tree("d")
      )
      tree7.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "g", "h", "i")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("h", Tree("i")))
      )
      tree7.insertBranch(List("a", "d", "g", "h")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("g", Tree("h")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "d", "e", "g")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("g"), Tree("f"))),
        Tree("g")
      )
    }

    "select a value by a path" in {
      tree0.selectValue(List(), _.length) shouldBe None
      tree1.selectValue(List(), _.length) shouldBe None
      tree1.selectValue(List(1), _.length) shouldBe Some("a")
      tree1.selectValue(List("a"), identity) shouldBe Some("a")
      tree1.selectValue(List("a", "b"), identity) shouldBe None
      tree2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree2.selectValue(List(1, 1), _.length) shouldBe Some("b")
      tree2.selectValue(List(1), _.length) shouldBe Some("a")
      tree2.selectValue(List(1, 0), _.length) shouldBe None
      tree2.selectValue(List(0, 1), _.length) shouldBe None
      tree2.selectValue(List(0, 0), _.length) shouldBe None
      tree3_1.selectValue(List(), _.length) shouldBe None
      tree3_1.selectValue(List(1, 1, 1), _.length) shouldBe Some("c")
      tree3_1.selectValue(List(1, 1), _.length) shouldBe Some("b")
      tree3_1.selectValue(List(1), _.length) shouldBe Some("a")
      tree3_2.selectValue(List(), _.length) shouldBe None
      tree3_2.selectValue(List(1), _.length) shouldBe Some("a")
      tree3_2.selectValue(List(1, 1), _.length) shouldBe Some("c")
      tree3_2.selectValue(List("a"), identity) shouldBe Some("a")
      tree3_2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree3_2.selectValue(List("a", "c"), identity) shouldBe Some("c")
      tree3_2.selectValue(List("a", "d"), identity) shouldBe None
      tree3_2.selectValue(List("a", "a"), identity) shouldBe None
      tree3_2.selectValue(List("c", "a"), identity) shouldBe None
      tree3_2.selectValue(List("b", "a"), identity) shouldBe None
      tree4_1.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_1.selectValue(List("a", "b", "c"), identity) shouldBe Some("c")
      tree4_1.selectValue(List("a", "b", "c", "d"), identity) shouldBe Some("d")
      tree4_1.selectValue(List("a", "c"), identity) shouldBe None
      tree4_1.selectValue(List("a", "d"), identity) shouldBe None
      tree4_2.selectValue(List(), identity) shouldBe None
      tree4_2.selectValue(List("a"), identity) shouldBe Some("a")
      tree4_2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_2.selectValue(List("a", "b", "d"), identity) shouldBe None
      tree4_2.selectValue(List("a", "b", "c"), identity) shouldBe Some("c")
      tree4_2.selectValue(List("a", "d"), identity) shouldBe Some("d")
      tree4_2.selectValue(List("a", "d", "c"), identity) shouldBe None
      tree4_2.selectValue(List("b", "c"), identity) shouldBe None
      tree4_2.selectValue(List("d"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_3.selectValue(List(), identity) shouldBe None
      tree4_3.selectValue(List("a"), identity) shouldBe Some("a")
      tree4_3.selectValue(List("a", "c"), identity) shouldBe Some("c")
      tree4_3.selectValue(List("a", "d"), identity) shouldBe Some("d")
      tree4_3.selectValue(List("a", "a"), identity) shouldBe None
      tree4_3.selectValue(List("a", "e"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b", "c"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b", "b"), identity) shouldBe None
      tree4_3.selectValue(List("a", "a", "a"), identity) shouldBe None
      tree9.selectValue(List("a", "b", "c", "d"), identity) shouldBe Some("d")
      tree9.selectValue(List("a", "e", "f", "g"), identity) shouldBe Some("g")
      tree9.selectValue(List("a", "e", "h", "i"), identity) shouldBe Some("i")
      tree9.selectValue(List("a", "e", "f", "i"), identity) shouldBe None
    }

    "select a tree by a path" in {
      tree0.selectTree(List()) shouldBe None
      tree0.selectTree(List("a")) shouldBe None
      tree1.selectTree(List("a")) shouldBe Some(Tree("a"))
      tree1.selectTree(List("a", "b")) shouldBe None
      tree2.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b"), Tree("c")))
      tree3_2.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List("a", "c")) shouldBe Some(Tree("c"))
      tree3_2.selectTree(List("a", "d")) shouldBe None
      tree3_2.selectTree(List("a", "a")) shouldBe None
      tree3_2.selectTree(List("c", "a")) shouldBe None
      tree3_2.selectTree(List("b", "a")) shouldBe None
      tree4_1.selectTree(List("a", "b")) shouldBe Some(Tree("b", Tree("c", Tree("d"))))
      tree4_1.selectTree(List("a", "b", "c")) shouldBe Some(Tree("c", Tree("d")))
      tree4_1.selectTree(List("a", "b", "c", "d")) shouldBe Some(Tree("d"))
      tree4_1.selectTree(List("a", "c")) shouldBe None
      tree4_1.selectTree(List("a", "d")) shouldBe None
      tree4_2.selectTree(List()) shouldBe None
      tree4_2.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b", Tree("c")), Tree("d")))
      tree4_2.selectTree(List("a", "b")) shouldBe Some(Tree("b", Tree("c")))
      tree4_2.selectTree(List("a", "b", "d")) shouldBe None
      tree4_2.selectTree(List("a", "b", "c")) shouldBe Some(Tree("c"))
      tree4_2.selectTree(List("a", "d")) shouldBe Some(Tree("d"))
      tree4_2.selectTree(List("a", "d", "c")) shouldBe None
      tree4_2.selectTree(List("b", "c")) shouldBe None
      tree4_2.selectTree(List("d")) shouldBe None
      tree4_3.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree4_3.selectTree(List()) shouldBe None
      tree4_3.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b"), Tree("c"), Tree("d")))
      tree4_3.selectTree(List("a", "c")) shouldBe Some(Tree("c"))
      tree4_3.selectTree(List("a", "d")) shouldBe Some(Tree("d"))
      tree4_3.selectTree(List("a", "a")) shouldBe None
      tree4_3.selectTree(List("a", "e")) shouldBe None
      tree4_3.selectTree(List("a", "b", "c")) shouldBe None
      tree4_3.selectTree(List("a", "b", "b")) shouldBe None
      tree4_3.selectTree(List("a", "a", "a")) shouldBe None
      tree9.selectTree(List("a", "b", "c", "d")) shouldBe Some(Tree("d"))
      tree9.selectTree(List("a", "e", "f", "g")) shouldBe Some(Tree("g"))
      tree9.selectTree(List("a", "e", "h", "i")) shouldBe Some(Tree("i"))
      tree9.selectTree(List("a", "e", "f", "i")) shouldBe None
    }

    "select a tree by path using extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.selectTree(List(), codeF) shouldBe None
      tree0.selectTree(List(0), codeF) shouldBe None
      tree1.selectTree(List(), codeF) shouldBe None
      tree1.selectTree(List(97), codeF) shouldBe Some(Tree("a"))
      tree1.selectTree(List(96), codeF) shouldBe None
      tree2.selectTree(List(97, 98), codeF) shouldBe Some(Tree("b"))
      tree2.selectTree(List(97, 97), codeF) shouldBe None
      tree2.selectTree(List(98, 97), codeF) shouldBe None
      tree3_2.selectTree(List(97, 98), codeF) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List(97, 99), codeF) shouldBe Some(Tree("c"))
      tree3_2.selectTree(List(97), codeF) shouldBe Some(Tree("a", Tree("b"), Tree("c")))
      tree3_2.selectTree(List(97, 97), codeF) shouldBe None
      tree3_2.selectTree(List(98), codeF) shouldBe None
      tree3_2.selectTree(List(), codeF) shouldBe None
    }

    "list all nodes" in {
      tree0.values shouldBe Nil
      tree1.values shouldBe List("a")
      tree2.values shouldBe List("a", "b")
      tree3_1.values shouldBe List("a", "b", "c")
      tree3_2.values shouldBe List("a", "b", "c")
      tree4_1.values shouldBe List("a", "b", "c", "d")
      tree4_2.values shouldBe List("a", "b", "c", "d")
      tree4_3.values shouldBe List("a", "b", "c", "d")
      tree7.values shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.values shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "iterate over nodes with filter" in {
      tree0.valueIterator(all).toList shouldBe Nil
      tree1.valueIterator(all).toList shouldBe List("a")
      tree2.valueIterator(all).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all).toList shouldBe List("a", "b", "c", "d")
      tree7.valueIterator(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueIterator(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valueIterator(none).toList shouldBe Nil
      tree1.valueIterator(none).toList shouldBe Nil
      tree2.valueIterator(none).toList shouldBe Nil
      tree3_1.valueIterator(none).toList shouldBe Nil
      tree3_2.valueIterator(none).toList shouldBe Nil
      tree4_1.valueIterator(none).toList shouldBe Nil
      tree4_2.valueIterator(none).toList shouldBe Nil
      tree4_3.valueIterator(none).toList shouldBe Nil
      tree7.valueIterator(none).toList shouldBe Nil
      tree9.valueIterator(none).toList shouldBe Nil

      tree0.valueIterator(even).toList shouldBe Nil
      tree1.valueIterator(even).toList shouldBe Nil
      tree2.valueIterator(even).toList shouldBe List("b")
      tree3_1.valueIterator(even).toList shouldBe List("b")
      tree3_2.valueIterator(even).toList shouldBe List("b")
      tree4_1.valueIterator(even).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even).toList shouldBe List("b", "d")
      tree7.valueIterator(even).toList shouldBe List("b", "d", "f")
      tree9.valueIterator(even).toList shouldBe List("b", "d", "f", "h")

      tree0.valueIterator(odd).toList shouldBe Nil
      tree1.valueIterator(odd).toList shouldBe List("a")
      tree2.valueIterator(odd).toList shouldBe List("a")
      tree3_1.valueIterator(odd).toList shouldBe List("a", "c")
      tree3_2.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_1.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_2.valueIterator(odd).toList shouldBe List("a", "c")
      tree4_3.valueIterator(odd).toList shouldBe List("a", "c")
      tree7.valueIterator(odd).toList shouldBe List("a", "c", "e", "g")
      tree9.valueIterator(odd).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "iterate over nodes with filter and maxDepth" in {
      tree0.valueIterator(all, 0).toList shouldBe Nil
      tree0.valueIterator(all, 1).toList shouldBe Nil
      tree1.valueIterator(all, 0).toList shouldBe Nil
      tree1.valueIterator(all, 1).toList shouldBe List("a")
      tree1.valueIterator(all, 2).toList shouldBe List("a")
      tree2.valueIterator(all, 0).toList shouldBe Nil
      tree2.valueIterator(all, 1).toList shouldBe List("a")
      tree2.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree2.valueIterator(all, 3).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all, 0).toList shouldBe Nil
      tree3_1.valueIterator(all, 1).toList shouldBe List("a")
      tree3_1.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree3_1.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree3_1.valueIterator(all, 4).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 0).toList shouldBe Nil
      tree3_2.valueIterator(all, 1).toList shouldBe List("a")
      tree3_2.valueIterator(all, 2).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree3_2.valueIterator(all, 4).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all, 0).toList shouldBe Nil
      tree4_1.valueIterator(all, 1).toList shouldBe List("a")
      tree4_1.valueIterator(all, 2).toList shouldBe List("a", "b")
      tree4_1.valueIterator(all, 3).toList shouldBe List("a", "b", "c")
      tree4_1.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all, 0).toList shouldBe Nil
      tree4_2.valueIterator(all, 1).toList shouldBe List("a")
      tree4_2.valueIterator(all, 2).toList shouldBe List("a", "b", "d")
      tree4_2.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all, 0).toList shouldBe Nil
      tree4_3.valueIterator(all, 1).toList shouldBe List("a")
      tree4_3.valueIterator(all, 2).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d")
      tree7.valueIterator(all, 0).toList shouldBe Nil
      tree7.valueIterator(all, 1).toList shouldBe List("a")
      tree7.valueIterator(all, 2).toList shouldBe List("a", "b", "d", "g")
      tree7.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "d", "e", "g")
      tree7.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueIterator(all, 0).toList shouldBe Nil
      tree9.valueIterator(all, 1).toList shouldBe List("a")
      tree9.valueIterator(all, 2).toList shouldBe List("a", "b", "e")
      tree9.valueIterator(all, 3).toList shouldBe List("a", "b", "c", "e", "f", "h")
      tree9.valueIterator(all, 4).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
      tree9.valueIterator(all, 5).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valueIterator(none, 10).toList shouldBe Nil
      tree1.valueIterator(none, 10).toList shouldBe Nil
      tree2.valueIterator(none, 10).toList shouldBe Nil
      tree3_1.valueIterator(none, 10).toList shouldBe Nil
      tree3_2.valueIterator(none, 10).toList shouldBe Nil
      tree4_1.valueIterator(none, 10).toList shouldBe Nil
      tree4_2.valueIterator(none, 10).toList shouldBe Nil
      tree4_3.valueIterator(none, 10).toList shouldBe Nil
      tree7.valueIterator(none, 10).toList shouldBe Nil
      tree9.valueIterator(none, 10).toList shouldBe Nil

      tree0.valueIterator(even, 10).toList shouldBe Nil
      tree1.valueIterator(even, 10).toList shouldBe Nil
      tree2.valueIterator(even, 1).toList shouldBe Nil
      tree2.valueIterator(even, 2).toList shouldBe List("b")
      tree3_1.valueIterator(even, 1).toList shouldBe Nil
      tree3_1.valueIterator(even, 2).toList shouldBe List("b")
      tree3_1.valueIterator(even, 3).toList shouldBe List("b")
      tree3_2.valueIterator(even, 0).toList shouldBe Nil
      tree3_2.valueIterator(even, 1).toList shouldBe Nil
      tree3_2.valueIterator(even, 2).toList shouldBe List("b")
      tree3_2.valueIterator(even, 3).toList shouldBe List("b")
      tree4_1.valueIterator(even, 0).toList shouldBe Nil
      tree4_1.valueIterator(even, 1).toList shouldBe Nil
      tree4_1.valueIterator(even, 2).toList shouldBe List("b")
      tree4_1.valueIterator(even, 3).toList shouldBe List("b")
      tree4_1.valueIterator(even, 4).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even, 0).toList shouldBe Nil
      tree4_2.valueIterator(even, 1).toList shouldBe Nil
      tree4_2.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree4_2.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even, 0).toList shouldBe Nil
      tree4_3.valueIterator(even, 1).toList shouldBe Nil
      tree4_3.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree4_3.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 1).toList shouldBe Nil
      tree7.valueIterator(even, 2).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 3).toList shouldBe List("b", "d")
      tree7.valueIterator(even, 4).toList shouldBe List("b", "d", "f")
      tree9.valueIterator(even, 1).toList shouldBe Nil
      tree9.valueIterator(even, 2).toList shouldBe List("b")
      tree9.valueIterator(even, 3).toList shouldBe List("b", "f", "h")
      tree9.valueIterator(even, 4).toList shouldBe List("b", "d", "f", "h")
      tree9.valueIterator(even, 5).toList shouldBe List("b", "d", "f", "h")
    }

    "stream all values" in {
      tree0.valueStream.toList shouldBe Nil
      tree1.valueStream.toList shouldBe List("a")
      tree2.valueStream.toList shouldBe List("a", "b")
      tree3_1.valueStream.toList shouldBe List("a", "b", "c")
      tree3_2.valueStream.toList shouldBe List("a", "b", "c")
      tree4_1.valueStream.toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueStream.toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueStream.toList shouldBe List("a", "b", "c", "d")
      tree7.valueStream.toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueStream.toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "stream filtered nodes" in {
      tree0.valueStream(all).toList shouldBe Nil
      tree1.valueStream(all).toList shouldBe List("a")
      tree2.valueStream(all).toList shouldBe List("a", "b")
      tree3_1.valueStream(all).toList shouldBe List("a", "b", "c")
      tree3_2.valueStream(all).toList shouldBe List("a", "b", "c")
      tree4_1.valueStream(all).toList shouldBe List("a", "b", "c", "d")
      tree4_2.valueStream(all).toList shouldBe List("a", "b", "c", "d")
      tree4_3.valueStream(all).toList shouldBe List("a", "b", "c", "d")
      tree7.valueStream(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      tree9.valueStream(all).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      tree0.valueStream(none).toList shouldBe Nil
      tree1.valueStream(none).toList shouldBe Nil
      tree2.valueStream(none).toList shouldBe Nil
      tree3_1.valueStream(none).toList shouldBe Nil
      tree3_2.valueStream(none).toList shouldBe Nil
      tree4_1.valueStream(none).toList shouldBe Nil
      tree4_2.valueStream(none).toList shouldBe Nil
      tree4_3.valueStream(none).toList shouldBe Nil
      tree7.valueStream(none).toList shouldBe Nil
      tree9.valueStream(none).toList shouldBe Nil

      tree0.valueStream(even).toList shouldBe Nil
      tree1.valueStream(even).toList shouldBe Nil
      tree2.valueStream(even).toList shouldBe List("b")
      tree3_1.valueStream(even).toList shouldBe List("b")
      tree3_2.valueStream(even).toList shouldBe List("b")
      tree4_1.valueStream(even).toList shouldBe List("b", "d")
      tree4_2.valueStream(even).toList shouldBe List("b", "d")
      tree4_3.valueStream(even).toList shouldBe List("b", "d")
      tree7.valueStream(even).toList shouldBe List("b", "d", "f")
      tree9.valueStream(even).toList shouldBe List("b", "d", "f", "h")

      tree0.valueStream(odd).toList shouldBe Nil
      tree1.valueStream(odd).toList shouldBe List("a")
      tree2.valueStream(odd).toList shouldBe List("a")
      tree3_1.valueStream(odd).toList shouldBe List("a", "c")
      tree3_2.valueStream(odd).toList shouldBe List("a", "c")
      tree4_1.valueStream(odd).toList shouldBe List("a", "c")
      tree4_2.valueStream(odd).toList shouldBe List("a", "c")
      tree4_3.valueStream(odd).toList shouldBe List("a", "c")
      tree7.valueStream(odd).toList shouldBe List("a", "c", "e", "g")
      tree9.valueStream(odd).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "list all branches" in {
      tree0.branches shouldBe Nil
      tree1.branches shouldBe List(List("a"))
      tree2.branches shouldBe List(List("a", "b"))
      tree3_1.branches shouldBe List(List("a", "b", "c"))
      tree3_2.branches shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branches shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branches shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branches shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branches shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      tree9.branches shouldBe List(List("a", "b", "c", "d"), List("a", "e", "f", "g"), List("a", "e", "h", "i"))
    }

    "iterate over branches with filter" in {
      tree0.branchIterator(all).map(_.toList).toList shouldBe Nil
      tree1.branchIterator(all).map(_.toList).toList shouldBe List(List("a"))
      tree2.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchIterator(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branchIterator(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchIterator(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      tree0.branchIterator(none).toList shouldBe Nil
      tree1.branchIterator(none).toList shouldBe Nil
      tree2.branchIterator(none).toList shouldBe Nil
      tree3_1.branchIterator(none).toList shouldBe Nil
      tree3_2.branchIterator(none).toList shouldBe Nil
      tree4_1.branchIterator(none).toList shouldBe Nil
      tree4_2.branchIterator(none).toList shouldBe Nil
      tree4_3.branchIterator(none).toList shouldBe Nil
      tree7.branchIterator(none).toList shouldBe Nil
      tree9.branchIterator(none).toList shouldBe Nil

      tree0.branchIterator(_.size > 3).toList shouldBe Nil
      tree1.branchIterator(_.size > 1).toList shouldBe Nil
      tree2.branchIterator(_.size > 1).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchIterator(_.last == "c").map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchIterator(_.last == "c").map(_.toList).toList shouldBe List(List("a", "c"))
      tree4_2.branchIterator(_.last == "d").map(_.toList).toList shouldBe List(List("a", "d"))
      tree4_2.branchIterator(_.size > 2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree7.branchIterator(_.size > 3).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
      tree9.branchIterator(_.size > 3).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchIterator(_.toList.contains("e")).map(_.toList).toList shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchIterator(_.toList.contains("h")).map(_.toList).toList shouldBe List(List("a", "e", "h", "i"))
      tree9.branchIterator(_.size < 3).map(_.toList).toList shouldBe Nil
    }

    "stream all branches" in {
      tree0.branchStream shouldBe Nil
      tree1.branchStream shouldBe List(List("a"))
      tree2.branchStream shouldBe List(List("a", "b"))
      tree3_1.branchStream shouldBe List(List("a", "b", "c"))
      tree3_2.branchStream shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchStream shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchStream shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchStream shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branchStream shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      tree9.branchStream shouldBe List(List("a", "b", "c", "d"), List("a", "e", "f", "g"), List("a", "e", "h", "i"))
    }

    "stream filtered branches" in {
      tree0.branchStream(all).map(_.toList).toList shouldBe Nil
      tree1.branchStream(all).map(_.toList).toList shouldBe List(List("a"))
      tree2.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      tree4_1.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      tree4_2.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      tree4_3.branchStream(all).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      tree7.branchStream(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      tree9.branchStream(all).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      tree0.branchStream(none).toList shouldBe Nil
      tree1.branchStream(none).toList shouldBe Nil
      tree2.branchStream(none).toList shouldBe Nil
      tree3_1.branchStream(none).toList shouldBe Nil
      tree3_2.branchStream(none).toList shouldBe Nil
      tree4_1.branchStream(none).toList shouldBe Nil
      tree4_2.branchStream(none).toList shouldBe Nil
      tree4_3.branchStream(none).toList shouldBe Nil
      tree7.branchStream(none).toList shouldBe Nil
      tree9.branchStream(none).toList shouldBe Nil

      tree0.branchStream(_.size > 3).toList shouldBe Nil
      tree1.branchStream(_.size > 1).toList shouldBe Nil
      tree2.branchStream(_.size > 1).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchStream(_.last == "c").map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchStream(_.last == "c").map(_.toList).toList shouldBe List(List("a", "c"))
      tree4_2.branchStream(_.last == "d").map(_.toList).toList shouldBe List(List("a", "d"))
      tree4_2.branchStream(_.size > 2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree7.branchStream(_.size > 3).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
      tree9.branchStream(_.size > 3).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchStream(_.toList.contains("e")).map(_.toList).toList shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      tree9.branchStream(_.toList.contains("h")).map(_.toList).toList shouldBe List(List("a", "e", "h", "i"))
      tree9.branchStream(_.size < 3).map(_.toList).toList shouldBe Nil
    }

    "list all sub-trees" in {
      tree0.trees shouldBe Nil
      tree1.trees shouldBe List(tree1)
      tree2.trees shouldBe List(tree2, Tree("b"))
      tree3_1.trees shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.trees shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.trees shouldBe List(tree4_1, Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("d")), Tree("d"))
      tree4_2.trees shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.trees shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.trees.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.trees.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin
    }

    "iterate over filtered subtrees" in {
      tree0.treeIterator(all).toList shouldBe Nil
      tree1.treeIterator(all).toList shouldBe List(tree1)
      tree2.treeIterator(all).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeIterator(all).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeIterator(all).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeIterator(all).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treeIterator(all).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeIterator(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeIterator(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treeIterator(none).toList shouldBe Nil
      tree1.treeIterator(none).toList shouldBe Nil
      tree2.treeIterator(none).toList shouldBe Nil
      tree3_1.treeIterator(none).toList shouldBe Nil
      tree3_2.treeIterator(none).toList shouldBe Nil
      tree4_1.treeIterator(none).toList shouldBe Nil
      tree4_2.treeIterator(none).toList shouldBe Nil
      tree4_3.treeIterator(none).toList shouldBe Nil
      tree7.treeIterator(none).toList shouldBe Nil
      tree9.treeIterator(none).toList shouldBe Nil

      tree0.treeIterator(_.size > 0).toList shouldBe Nil
      tree1.treeIterator(_.size > 0).toList shouldBe List(tree1)
      tree2.treeIterator(_.size > 0).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treeIterator(_.size < 2).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treeIterator(_.height == 2).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))
      tree9.treeIterator(_.height == 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treeIterator(_.height > 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treeIterator(_.width >= 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }

    "iterate over filtered subtrees with depth limit" in {
      tree0.treeIterator(all, 0).toList shouldBe Nil
      tree0.treeIterator(all, 1).toList shouldBe Nil
      tree1.treeIterator(all, 0).toList shouldBe Nil
      tree1.treeIterator(all, 1).toList shouldBe List(tree1)
      tree1.treeIterator(all, 2).toList shouldBe List(tree1)
      tree2.treeIterator(all, 0).toList shouldBe Nil
      tree2.treeIterator(all, 1).toList shouldBe List(tree2)
      tree2.treeIterator(all, 2).toList shouldBe List(tree2, Tree("b"))
      tree2.treeIterator(all, 3).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeIterator(all, 0).toList shouldBe Nil
      tree3_1.treeIterator(all, 1).toList shouldBe List(tree3_1)
      tree3_1.treeIterator(all, 2).toList shouldBe List(tree3_1, Tree("b", Tree("c")))
      tree3_1.treeIterator(all, 3).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeIterator(all, 0).toList shouldBe Nil
      tree3_2.treeIterator(all, 1).toList shouldBe List(tree3_2)
      tree3_2.treeIterator(all, 2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeIterator(all, 0).toList shouldBe Nil
      tree4_1.treeIterator(all, 1).toList shouldBe List(tree4_1)
      tree4_1.treeIterator(all, 2).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_1.treeIterator(all, 3).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d"))
      )
      tree4_1.treeIterator(all, 4).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treeIterator(all, 0).toList shouldBe Nil
      tree4_2.treeIterator(all, 1).toList shouldBe List(tree4_2)
      tree4_2.treeIterator(all, 2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("d"))
      tree4_2.treeIterator(all, 3).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_2.treeIterator(all, 4).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all, 0).toList shouldBe Nil
      tree4_3.treeIterator(all, 1).toList shouldBe List(tree4_3)
      tree4_3.treeIterator(all, 2).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all, 3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeIterator(all, 0).toList shouldBe Nil
      tree7.treeIterator(all, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]""".stripMargin
      tree7.treeIterator(all, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]""".stripMargin
      tree7.treeIterator(all, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[g]""".stripMargin
      tree7.treeIterator(all, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeIterator(all, 0).toList shouldBe Nil
      tree9.treeIterator(all, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]""".stripMargin
      tree9.treeIterator(all, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treeIterator(all, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treeIterator(all, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treeIterator(none, 10).toList shouldBe Nil
      tree1.treeIterator(none, 10).toList shouldBe Nil
      tree2.treeIterator(none, 10).toList shouldBe Nil
      tree3_1.treeIterator(none, 10).toList shouldBe Nil
      tree3_2.treeIterator(none, 10).toList shouldBe Nil
      tree4_1.treeIterator(none, 10).toList shouldBe Nil
      tree4_2.treeIterator(none, 10).toList shouldBe Nil
      tree4_3.treeIterator(none, 10).toList shouldBe Nil
      tree7.treeIterator(none, 10).toList shouldBe Nil
      tree9.treeIterator(none, 10).toList shouldBe Nil
    }

    "stream all subtrees" in {
      tree0.treeStream.toList shouldBe Nil
      tree1.treeStream.toList shouldBe List(tree1)
      tree2.treeStream.toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeStream.toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeStream.toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeStream.toList shouldBe List(tree4_1, Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("d")), Tree("d"))
      tree4_2.treeStream.toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeStream.toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeStream.toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeStream.toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin
    }

    "stream filtered subtrees" in {
      tree0.treeStream(all).toList shouldBe Nil
      tree1.treeStream(all).toList shouldBe List(tree1)
      tree2.treeStream(all).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeStream(all).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeStream(all).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeStream(all).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treeStream(all).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeStream(all).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeStream(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeStream(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treeStream(none).toList shouldBe Nil
      tree1.treeStream(none).toList shouldBe Nil
      tree2.treeStream(none).toList shouldBe Nil
      tree3_1.treeStream(none).toList shouldBe Nil
      tree3_2.treeStream(none).toList shouldBe Nil
      tree4_1.treeStream(none).toList shouldBe Nil
      tree4_2.treeStream(none).toList shouldBe Nil
      tree4_3.treeStream(none).toList shouldBe Nil
      tree7.treeStream(none).toList shouldBe Nil
      tree9.treeStream(none).toList shouldBe Nil

      tree0.treeStream(_.size > 0).toList shouldBe Nil
      tree1.treeStream(_.size > 0).toList shouldBe List(tree1)
      tree2.treeStream(_.size > 0).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treeStream(_.size < 2).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treeStream(_.height == 2).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))
      tree9.treeStream(_.height == 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treeStream(_.height > 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treeStream(_.width >= 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }

    "map all nodes" in {
      val f: String => String = _ + "0"
      tree0.map(f).showAsGraph() shouldBe ""
      tree1.map(f).showAsGraph() shouldBe "a0"
      tree2.map(f).showAsGraph() shouldBe """a0 > b0"""
      tree3_1.map(f).showAsGraph() shouldBe """a0 > b0 > c0"""
      tree3_2.map(f).showAsGraph() shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin
      tree7.map(f).showAsGraph() shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }

    "flatMap all nodes" in {
      val f: String => Tree[String] = x => Tree(x, Tree(x + "0"))
      tree0.flatMap(f).showAsGraph() shouldBe ""
      tree1.flatMap(f).showAsGraph() shouldBe "a > a0"
      tree2.flatMap(f).showAsGraph() shouldBe
        """a > b > b0
          |a > a0""".stripMargin
      tree3_1.flatMap(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > a0""".stripMargin
      tree3_2.flatMap(f).showAsGraph() shouldBe
        """a > b > b0
          |a > c > c0
          |a > a0""".stripMargin
      tree7.flatMap(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > d > e > f > f0
          |a > d > e > e0
          |a > d > d0
          |a > g > g0
          |a > a0""".stripMargin
    }

    "transform a tree using for-comprehension" in {
      (for {
        n       <- tree9
        subtree <- Tree(n, Tree(n + n), Tree(n + n + n))
      } yield subtree).showAsGraph() shouldBe
        """a > b > c > d > dd
          |a > b > c > d > ddd
          |a > b > c > cc
          |a > b > c > ccc
          |a > b > bb
          |a > b > bbb
          |a > e > f > g > gg
          |a > e > f > g > ggg
          |a > e > f > ff
          |a > e > f > fff
          |a > e > h > i > ii
          |a > e > h > i > iii
          |a > e > h > hh
          |a > e > h > hhh
          |a > e > ee
          |a > e > eee
          |a > aa
          |a > aaa""".stripMargin
    }

    "serialize a tree to a list of (numberOfChildren, value) pairs" in {
      tree0.toPairsIterator shouldBe Iterator.empty
      TreeBuilder.fromPairsIterator(Iterator.empty) shouldBe List(Tree.empty)
      tree1.toPairsIterator.toList shouldBe List((0, "a"))
      tree2.toPairsIterator.toList shouldBe List((0, "b"), (1, "a"))
      tree3_1.toPairsIterator.toList shouldBe List((0, "c"), (1, "b"), (1, "a"))
      tree3_2.toPairsIterator.toList shouldBe List((0, "c"), (0, "b"), (2, "a"))
      tree4_1.toPairsIterator.toList shouldBe List((0, "d"), (1, "c"), (1, "b"), (1, "a"))
      tree4_2.toPairsIterator.toList shouldBe List((0, "d"), (0, "c"), (1, "b"), (2, "a"))
      tree4_3.toPairsIterator.toList shouldBe List((0, "d"), (0, "c"), (0, "b"), (3, "a"))
    }

    "serialize a tree to a pair of arrays and deserialize it back using fromArrays" in {
      val (structure0, values0) = tree0.toArrays
      structure0.length shouldBe 0
      values0.length shouldBe 0
      TreeBuilder.fromArrays(structure0, values0) shouldBe List(tree0)

      val (structure1, values1) = tree1.toArrays
      structure1.length shouldBe 1
      values1.length shouldBe 1
      TreeBuilder.fromArrays(structure1, values1) shouldBe List(tree1)

      val (structure2, values2) = tree2.toArrays
      structure2.length shouldBe 2
      structure2 shouldBe Array(0, 1)
      values2.length shouldBe 2
      values2 shouldBe Array("b", "a")
      TreeBuilder.fromArrays(structure2, values2) shouldBe List(tree2)

      val (structure3, values3) = tree3_1.toArrays
      structure3.length shouldBe 3
      structure3 shouldBe Array(0, 1, 1)
      values3.length shouldBe 3
      values3 shouldBe Array("c", "b", "a")
      TreeBuilder.fromArrays(structure3, values3) shouldBe List(tree3_1)

      val (structure3_2, values3_2) = tree3_2.toArrays
      structure3_2.length shouldBe 3
      structure3_2 shouldBe Array(0, 0, 2)
      values3_2.length shouldBe 3
      values3_2 shouldBe Array("c", "b", "a")
      TreeBuilder.fromArrays(structure3_2, values3_2) shouldBe List(tree3_2)

      val (structure4, values4) = tree4_1.toArrays
      structure4.length shouldBe 4
      structure4 shouldBe Array(0, 1, 1, 1)
      values4.length shouldBe 4
      values4 shouldBe Array("d", "c", "b", "a")
      TreeBuilder.fromArrays(structure4, values4) shouldBe List(tree4_1)

      val (structure4_2, values4_2) = tree4_2.toArrays
      structure4_2.length shouldBe 4
      structure4_2 shouldBe Array(0, 0, 1, 2)
      values4_2.length shouldBe 4
      values4_2 shouldBe Array("d", "c", "b", "a")
      TreeBuilder.fromArrays(structure4_2, values4_2) shouldBe List(tree4_2)

      val (structure7, values7) = tree7.toArrays
      structure7.length shouldBe 7
      structure7 shouldBe Array(0, 0, 1, 1, 0, 1, 3)
      values7.length shouldBe 7
      values7 shouldBe Array("g", "f", "e", "d", "c", "b", "a")
      TreeBuilder.fromArrays(structure7, values7) shouldBe List(tree7)
    }

    "serialize a tree to a structure array" in {
      tree0.toStructureArray shouldBe Array.empty[Int]
      tree1.toStructureArray shouldBe Array(0)
      tree2.toStructureArray shouldBe Array(0, 1)
      tree3_1.toStructureArray shouldBe Array(0, 1, 1)
      tree3_2.toStructureArray shouldBe Array(0, 0, 2)
      tree4_1.toStructureArray shouldBe Array(0, 1, 1, 1)
      tree4_2.toStructureArray shouldBe Array(0, 0, 1, 2)
      tree4_3.toStructureArray shouldBe Array(0, 0, 0, 3)
      tree7.toStructureArray shouldBe Array(0, 0, 1, 1, 0, 1, 3)
      tree9.toStructureArray shouldBe Array(0, 1, 0, 1, 2, 0, 1, 1, 2)
    }

    "visualize the branches of the tree" in {
      tree0.showAsArrays() shouldBe ""
      tree1.showAsArrays() shouldBe "[a]"
      tree2.showAsArrays() shouldBe "[a,b]"
      tree3_1.showAsArrays() shouldBe "[a,b,c]"
      tree3_2.showAsArrays() shouldBe "[a,b],[a,c]"
      tree4_1.showAsArrays() shouldBe "[a,b,c,d]"
      tree4_2.showAsArrays() shouldBe "[a,b,c],[a,d]"
      tree4_3.showAsArrays() shouldBe "[a,b],[a,c],[a,d]"
      tree7.showAsArrays() shouldBe "[a,b,c],[a,d,e,f],[a,g]"
      tree9.showAsArrays() shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
    }

    "visualize the branches of the tree limiting the depth" in {
      def mkStringWithMaxDepth(n: Int) = tree9.mkStringUsingBranches(_.toString, ",", ",", "[", "]", n)
      mkStringWithMaxDepth(0) shouldBe ""
      mkStringWithMaxDepth(1) shouldBe "[a]"
      mkStringWithMaxDepth(2) shouldBe "[a,b],[a,e]"
      mkStringWithMaxDepth(3) shouldBe "[a,b,c],[a,e,f],[a,e,h]"
      mkStringWithMaxDepth(4) shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
      mkStringWithMaxDepth(5) shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
    }

    "be equal to the other tree if both have same structure and content" in {
      tree0 shouldBe Tree()
      tree0.hashCode() shouldBe Tree().hashCode()
      tree0 shouldBe Tree()
      tree0 shouldBe Tree().deflated
      tree0.hashCode() shouldBe Tree().deflated.hashCode()

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
