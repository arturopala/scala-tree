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

  s"$name" should {

    "create an empty Tree" in {
      tree0.size shouldBe 0
      tree0.width shouldBe 0
      tree0.height shouldBe 0
      tree0.isLeaf shouldBe false
      tree0.childrenValues shouldBe Nil
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
      tree1.childrenValues shouldBe Nil
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
      tree2.childrenValues shouldBe List("b")
      tree2.countBranches(_.nonEmpty) shouldBe 1
      tree2.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree2) shouldBe "[a,b]"
      tree2.map(_ + 1) shouldBe Tree("a1", Tree("b1"))
    }

    "create a three nodes Tree" in {
      tree3_1.size shouldBe 3
      tree3_1.width shouldBe 1
      tree3_1.height shouldBe 3
      tree3_1.isLeaf shouldBe false
      tree3_1.childrenValues shouldBe List("b")
      tree3_1.countBranches(_.nonEmpty) shouldBe 1
      val arrays1 = showAsArrays(tree3_1)
      arrays1 shouldBe "[a,b,c]"
      val newTree3_1 = tree3_1.map(_ + 1)
      newTree3_1 shouldBe Tree("a1", Tree("b1", Tree("c1")))

      tree3_2.size shouldBe 3
      tree3_2.width shouldBe 2
      tree3_2.height shouldBe 2
      tree3_2.isLeaf shouldBe false
      tree3_2.childrenValues shouldBe List("b", "c")
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
      tree4_1.childrenValues shouldBe List("b")
      showAsArrays(tree4_1) shouldBe "[a,b,c,d]"
      tree4_1.map(_ + 1) shouldBe Tree("a1", Tree("b1", Tree("c1", Tree("d1"))))

      tree4_2.size shouldBe 4
      tree4_2.width shouldBe 2
      tree4_2.height shouldBe 3
      tree4_2.isLeaf shouldBe false
      tree4_2.childrenValues shouldBe List("b", "d")
      tree4_2.countBranches(_.nonEmpty) shouldBe 2
      showAsArrays(tree4_2) shouldBe
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
      tree9.countBranches(_.toSeq.contains("e")) shouldBe 2
      showAsArrays(tree9) shouldBe
        """[a,b,c,d]
          |[a,e,f,g]
          |[a,e,h,i]""".stripMargin
    }

    "check if tree contains a branch" in {
      tree0.containsBranch(List("a", "b")) shouldBe false
      tree9.containsBranch(List("a", "b", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "h", "i")) shouldBe true
      tree9.containsBranch(List("a", "e", "c")) shouldBe false
    }

    "check if tree contains a path" in {
      tree0.containsPath(List("a", "b")) shouldBe false
      tree9.containsPath(List("a", "b", "c")) shouldBe true
      tree9.containsPath(List("a", "e", "h", "i")) shouldBe true
      tree9.containsPath(List("a", "e", "c")) shouldBe false
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

    "select a node by path" in {
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

    "select an existing subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.selectTree(List(0, 1)) shouldBe Some(Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.containsPath(List(0, 1)) shouldBe true
      tree.containsBranch(List(0, 1)) shouldBe false
    }

    "try selecting non-existent subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.selectTree(List(1, 2)) shouldBe None
      tree.containsBranch(List(1, 2)) shouldBe false
    }

    "list all nodes" in {
      val tree = Tree.empty
      tree.valuesUnsafe shouldBe Nil
      val tree1 = Tree(0)
      tree1.valuesUnsafe shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.valuesUnsafe shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "list all nodes using tail safe method" in {
      val tree = Tree.empty
      tree.values shouldBe Nil
      val tree1 = Tree(0)
      tree1.values shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.values shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "iterate over nodes with filter" in {
      Tree.empty.valueIterator(_ => true).toList shouldBe Nil

      val tree1 = Tree(0)
      tree1.valueIterator(_ > 1).toList shouldBe Nil
      tree1.valueIterator(_ < 1).toList shouldBe List(0)

      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.valueIterator(_ >= 0).toList shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
      tree2.valueIterator(_ > 15).toList shouldBe List(20, 30, 21, 31, 22, 32)
      tree2.valueIterator(_ > 100).toList shouldBe Nil
      tree2.valueIterator(_ < 15).toList shouldBe List(0, 11, 12)
      tree2.valueIterator(_ < 0).toList shouldBe Nil
    }

    "stream all nodes" in {
      val tree = Tree.empty
      tree.valueStream.toList shouldBe Nil
      val tree1 = Tree(0)
      tree1.valueStream.toList shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.valueStream.toList shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "stream filtered nodes" in {
      val tree1 = Tree(0)
      tree1.valueStream(_ > 15).toList shouldBe Nil
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.valueStream(_ > 15).toList shouldBe List(20, 30, 21, 31, 22, 32)
    }

    "list all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin

      val branches: List[List[Int]] = tree.branchesUnsafe

      val graph = branches.map(_.mkString(" > ")).mkString("\n")
      graph should be(expected)
      graph shouldBe showAsGraph(tree)
      branches.forall(tree.containsBranch) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insertBranch(_)) shouldBe tree
    }

    "list all branches using tail safe method" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin

      val branches: List[List[Int]] = tree.branches

      val graph = branches.map(_.mkString(" > ")).mkString("\n")
      graph should be(expected)
      graph shouldBe showAsGraph(tree)
      branches.forall(tree.containsBranch) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insertBranch(_)) shouldBe tree
    }

    "iterate over branches with filter" in {
      tree0.branchIterator(_.size > 3).toList shouldBe Nil
      tree1.branchIterator(_.size > 1).toList shouldBe Nil
      tree2.branchIterator(_.size > 1).map(_.toList).toList shouldBe List(List("a", "b"))
      tree3_1.branchIterator(_.last == "c").map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree3_2.branchIterator(_.last == "c").map(_.toList).toList shouldBe List(List("a", "c"))
      tree4_2.branchIterator(_.last == "d").map(_.toList).toList shouldBe List(List("a", "d"))
      tree4_2.branchIterator(_.size > 2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      tree7.branchIterator(_.size > 3).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
    }

    "stream all branches" in {
      val tree0 = Tree()
      tree0.branchStream.toList shouldBe Nil

      val tree1 = Tree(0)
      tree1.branchStream.toList shouldBe List(List(0))

      val tree2 = Tree(0, Tree(1))
      tree2.branchStream.toList shouldBe List(List(0, 1))

      val tree3 = Tree(0, Tree(1), Tree(2))
      tree3.branchStream.toList shouldBe List(List(0, 1), List(0, 2))

      val tree4 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val graph = tree4.branchStream.map(_.mkString(" > ")).mkString("\n")
      graph shouldBe """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin
      graph shouldBe showAsGraph(tree4)
    }

    "list all sub-trees" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32
                       |
                       |11 > 20 > 30
                       |
                       |20 > 30
                       |
                       |30
                       |
                       |12 > 21 > 31
                       |12 > 22 > 32
                       |
                       |21 > 31
                       |
                       |31
                       |
                       |22 > 32
                       |
                       |32""".stripMargin

      val trees: List[Tree[Int]] = tree.treesUnsafe
      val treesGraph = trees.map(showAsGraph).mkString("\n\n")

      treesGraph should be(expected)
    }

    "iterate over filtered subtrees" in {
      tree0.treeIterator(_.size > 0).toList shouldBe Nil
      tree1.treeIterator(_.size > 0).toList shouldBe List(tree1)
      tree2.treeIterator(_.size > 0).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treeIterator(_.size < 2).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treeIterator(_.height == 2).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))

      val tree9 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree9.treeIterator(_.size == 2).toList shouldBe List(Tree(20, Tree(30)), Tree(21, Tree(31)), Tree(22, Tree(32)))
    }

    "stream filtered subtrees" in {
      tree0.treeStream(_.size > 0).toList shouldBe Nil
      tree1.treeStream(_.size > 0).toList shouldBe List(tree1)
      tree2.treeStream(_.size > 0).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treeStream(_.size < 2).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treeStream(_.height == 2).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))

      val tree9 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree9.treeStream(_.size == 2).toList shouldBe List(Tree(20, Tree(30)), Tree(21, Tree(31)), Tree(22, Tree(32)))
    }

    "map all nodes" in {
      val f: String => String = _ + "0"

      val result0 = tree0.map(f)
      showAsGraph(result0) shouldBe ""

      val result1 = tree1.map(f)
      showAsGraph(result1) shouldBe "a0"

      val result2 = tree2.map(f)
      showAsGraph(result2) shouldBe
        """a0 > b0""".stripMargin

      val result3_1 = tree3_1.map(f)
      showAsGraph(result3_1) shouldBe
        """a0 > b0 > c0""".stripMargin

      val result3_2 = tree3_2.map(f)
      showAsGraph(result3_2) shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin

      val result4 = tree7.map(f)
      showAsGraph(result4) shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }

    "mapUnsafe all nodes" in {
      val f: String => String = _ + "0"

      val result0 = tree0.mapUnsafe(f)
      showAsGraph(result0) shouldBe ""

      val result1 = tree1.mapUnsafe(f)
      showAsGraph(result1) shouldBe "a0"

      val result2 = tree2.mapUnsafe(f)
      showAsGraph(result2) shouldBe
        """a0 > b0""".stripMargin

      val result3_1 = tree3_1.mapUnsafe(f)
      showAsGraph(result3_1) shouldBe
        """a0 > b0 > c0""".stripMargin

      val result3_2 = tree3_2.mapUnsafe(f)
      showAsGraph(result3_2) shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin

      val result4 = tree7.mapUnsafe(f)
      showAsGraph(result4) shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }

    "flatMap all nodes" in {
      val f: String => Tree[String] = x => Tree(x, Tree(x + "0"))

      val result0 = tree0.flatMap(f)
      showAsGraph(result0) shouldBe ""

      val result1 = tree1.flatMap(f)
      showAsGraph(result1) shouldBe "a > a0"

      val result2 = tree2.flatMap(f)
      showAsGraph(result2) shouldBe
        """a > b > b0
          |a > a0""".stripMargin

      val result3_1 = tree3_1.flatMap(f)
      showAsGraph(result3_1) shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > a0""".stripMargin

      val result3_2 = tree3_2.flatMap(f)
      showAsGraph(result3_2) shouldBe
        """a > b > b0
          |a > c > c0
          |a > a0""".stripMargin

      val result4 = tree7.flatMap(f)
      showAsGraph(result4) shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > d > e > f > f0
          |a > d > e > e0
          |a > d > d0
          |a > g > g0
          |a > a0""".stripMargin
    }

    "transform a tree using for-comprehension" in {
      val tree = Tree(2, Tree(5, Tree(10)))
      val nodeList = for {
        n       <- tree
        subtree <- Tree(n, Tree(n * n))
      } yield subtree
      nodeList shouldBe Tree(2, Tree(5, Tree(10, Tree(100)), Tree(25)), Tree(4))
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
      val tree0: Tree[String] = Tree()
      val (structure0, values0) = tree0.toArrays
      structure0.length shouldBe 0
      values0.length shouldBe 0
      TreeBuilder.fromArrays(structure0, values0) shouldBe List(tree0)

      val tree1 = Tree(1)
      val (structure1, values1) = tree1.toArrays
      structure1.length shouldBe 1
      values1.length shouldBe 1
      TreeBuilder.fromArrays(structure1, values1) shouldBe List(tree1)

      val tree2 = Tree("a", Tree("b"))
      val (structure2, values2) = tree2.toArrays
      structure2.length shouldBe 2
      structure2 shouldBe Array(0, 1)
      values2.length shouldBe 2
      values2 shouldBe Array("b", "a")
      TreeBuilder.fromArrays(structure2, values2) shouldBe List(tree2)

      val tree3 = Tree("a", Tree("b"), Tree("c"))
      val (structure3, values3) = tree3.toArrays
      structure3.length shouldBe 3
      structure3 shouldBe Array(0, 0, 2)
      values3.length shouldBe 3
      values3 shouldBe Array("c", "b", "a")
      val t = TreeBuilder.fromArrays(structure3, values3)
      t shouldBe List(tree3)

      val tree4 = Tree("a", Tree("b", Tree("c")), Tree("d"))
      val (structure4, values4) = tree4.toArrays
      structure4.length shouldBe 4
      structure4 shouldBe Array(0, 0, 1, 2)
      values4.length shouldBe 4
      values4 shouldBe Array("d", "c", "b", "a")
      TreeBuilder.fromArrays(structure4, values4) shouldBe List(tree4)

      val tree7 = Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2"))), Tree("b3"))
      val (structure7, values7) = tree7.toArrays
      structure7.length shouldBe 7
      structure7 shouldBe Array(0, 0, 1, 1, 0, 1, 3)
      values7.length shouldBe 7
      values7 shouldBe Array("b3", "d2", "c2", "b2", "c1", "b1", "a")
      TreeBuilder.fromArrays(structure7, values7) shouldBe List(tree7)
      val tree10 = Tree(
        "a",
        Tree("b1", Tree("c1"), Tree("d1")),
        Tree("b2", Tree("c2", Tree("d2"))),
        Tree("b3"),
        Tree("b4", Tree("c4"))
      )
      val (structure10, values10) = tree10.toArrays
      structure10.length shouldBe 10
      structure10 shouldBe Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4)
      values10.length shouldBe 10
      values10 shouldBe Array("c4", "b4", "b3", "d2", "c2", "b2", "d1", "c1", "b1", "a")
      TreeBuilder.fromArraysHead(structure10, values10) shouldBe tree10
    }

    "serialize a tree to a structure array" in {
      Tree.empty.toStructureArray shouldBe Array.empty[Int]
      Tree(1).toStructureArray shouldBe Array(0)
      Tree(1, Tree(2)).toStructureArray shouldBe Array(0, 1)
      Tree(1, Tree(2, Tree(3))).toStructureArray shouldBe Array(0, 1, 1)
      Tree(1, Tree(2), Tree(3)).toStructureArray shouldBe Array(0, 0, 2)
      Tree(1, Tree(2), Tree(3, Tree(4))).toStructureArray shouldBe Array(0, 1, 0, 2)
      Tree(1, Tree(2, Tree(5)), Tree(3, Tree(4))).toStructureArray shouldBe Array(0, 1, 0, 1, 2)
      Tree(1, Tree(2, Tree(5)), Tree(3, Tree(4), Tree(6))).toStructureArray shouldBe Array(0, 0, 2, 0, 1, 2)
      Tree(1, Tree(2, Tree(5)), Tree(3, Tree(4), Tree(6)), Tree(7)).toStructureArray shouldBe Array(0, 0, 0, 2, 0, 1, 3)
    }

    "visualize the branches of the tree" in {
      val tree =
        Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2", Tree("e1"), Tree("e2")))), Tree("b3"))
      tree.mkStringUsingBranches(_.toString, ",", ",", "[", "]") shouldBe "[a,b1,c1],[a,b2,c2,d2,e1],[a,b2,c2,d2,e2],[a,b3]"
    }

    "visualize the branches of the tree limiting the depth" in {
      val tree =
        Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2", Tree("e1"), Tree("e2")))), Tree("b3"))
      def mkStringWithMaxDepth(n: Int) = tree.mkStringUsingBranches(_.toString, ",", ",", "[", "]", n)
      mkStringWithMaxDepth(0) shouldBe "[a]"
      mkStringWithMaxDepth(1) shouldBe "[a,b1],[a,b2],[a,b3]"
      mkStringWithMaxDepth(2) shouldBe "[a,b1,c1],[a,b2,c2],[a,b3]"
      mkStringWithMaxDepth(3) shouldBe "[a,b1,c1],[a,b2,c2,d2],[a,b3]"
      mkStringWithMaxDepth(4) shouldBe "[a,b1,c1],[a,b2,c2,d2,e1],[a,b2,c2,d2,e2],[a,b3]"
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
