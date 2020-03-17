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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.github.arturopala.tree.Tree.Show._

class TreeSpec extends AnyWordSpec with Matchers {

  "Tree" should {
    "create an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      tree.size shouldBe 0
      tree.leafsSize shouldBe 0
      tree.isLeaf shouldBe false
      tree.children shouldBe Nil
      tree.countBranches(_.nonEmpty) shouldBe 0
      tree.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree) shouldBe ""
      tree.map(_ + 1) shouldBe Tree.empty
    }

    "create a single node Tree" in {
      val tree1 = Tree(0)
      tree1 shouldBe Tree(0)
      tree1.size shouldBe 1
      tree1.leafsSize shouldBe 1
      tree1.isLeaf shouldBe true
      tree1.children shouldBe Nil
      tree1.countBranches(_.nonEmpty) shouldBe 1
      tree1.countBranches(_.isEmpty) shouldBe 1
      showAsArrays(tree1) shouldBe "[0]"
      tree1.map(_ + 1) shouldBe Tree(1)
    }

    "create a double node Tree" in {
      val tree1 = Tree(0, Tree(1))
      tree1.size shouldBe 2
      tree1.leafsSize shouldBe 1
      tree1.isLeaf shouldBe false
      tree1.children shouldBe List(Tree(1))
      tree1.countBranches(_.nonEmpty) shouldBe 1
      showAsArrays(tree1) shouldBe "[0,1]"
      tree1.map(_ + 1) shouldBe Tree(1, Tree(2))
    }

    "create a three nodes Tree" in {
      val tree1 = Tree(0, Tree(1, Tree(2)))
      tree1.size shouldBe 3
      tree1.leafsSize shouldBe 1
      tree1.isLeaf shouldBe false
      tree1.children shouldBe List(Tree(1, Tree(2)))
      tree1.countBranches(_.nonEmpty) shouldBe 1
      val arrays1 = showAsArrays(tree1)
      arrays1 shouldBe "[0,1,2]"
      tree1.map(_ + 1) shouldBe Tree(1, Tree(2, Tree(3)))

      val tree2 = Tree(0, Tree(10), Tree(11))
      tree2.size shouldBe 3
      tree2.leafsSize shouldBe 2
      tree2.isLeaf shouldBe false
      tree2.children shouldBe List(Tree(10), Tree(11))
      tree2.countBranches(_.nonEmpty) shouldBe 2
      val arrays2 = showAsArrays(tree2)
      arrays2 shouldBe
        """[0,10]
          |[0,11]""".stripMargin
      tree2.map(_ + 1) shouldBe Tree(1, Tree(11), Tree(12))
    }

    "create a four nodes Tree" in {
      val tree1 = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree1.size shouldBe 4
      tree1.leafsSize shouldBe 1
      tree1.isLeaf shouldBe false
      tree1.countBranches(_.nonEmpty) shouldBe 1
      tree1.children shouldBe List(Tree(1, Tree(2, Tree(3))))
      showAsArrays(tree1) shouldBe "[0,1,2,3]"
      tree1.map(_ + 1) shouldBe Tree(1, Tree(2, Tree(3, Tree(4))))

      val tree2 = Tree(0, Tree(1, Tree(20), Tree(21)))
      tree2.size shouldBe 4
      tree2.leafsSize shouldBe 2
      tree2.isLeaf shouldBe false
      tree2.children shouldBe List(Tree(1, Tree(20), Tree(21)))
      tree2.countBranches(_.nonEmpty) shouldBe 2
      showAsArrays(tree2) shouldBe
        """[0,1,20]
          |[0,1,21]""".stripMargin
      tree2.map(_ + 1) shouldBe Tree(1, Tree(2, Tree(21), Tree(22)))

      val tree3 = Tree(0, Tree(10), Tree(11), Tree(12))
      tree3.size shouldBe 4
      tree3.leafsSize shouldBe 3
      tree3.isLeaf shouldBe false
      tree3.children shouldBe List(Tree(10), Tree(11), Tree(12))
      tree3.countBranches(_.nonEmpty) shouldBe 3
      tree3.countBranches(_.contains(11)) shouldBe 1
      showAsArrays(tree3) shouldBe
        """[0,10]
          |[0,11]
          |[0,12]""".stripMargin
      tree3.map(_ + 1) shouldBe Tree(1, Tree(11), Tree(12), Tree(13))
    }

    "create a multi-branch Tree" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree.size shouldBe 9
      tree.leafsSize shouldBe 3
      tree.isLeaf shouldBe false
      tree.countBranches(_.nonEmpty) shouldBe 3
      tree.countBranches(_.contains(12)) shouldBe 2
      showAsArrays(tree) shouldBe
        """[0,11,20,30]
          |[0,12,21,31]
          |[0,12,22,32]""".stripMargin
    }

    "check if the path exists" in {
      val tree = Tree.empty
      tree.contains(List(0, 1)) shouldBe false

      val tree1 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree1.contains(List(0, 11)) shouldBe true
      tree1.contains(List(0, 11, 20)) shouldBe true
      tree1.contains(List(0, 11, 30)) shouldBe false
    }

    "insert new node to an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      val tree2 = tree.insert(0)
      tree2 shouldBe Tree(0)
      tree2.size shouldBe 1
      tree2.leafsSize shouldBe 1
    }

    "insert new node to a single node Tree" in {
      val tree = Tree(0)
      val tree2 = tree.insert(1)
      tree2 shouldBe Tree(0, Tree(1))
      tree2.size shouldBe 2
      tree2.leafsSize shouldBe 1
    }

    "insert new branch to an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      val tree2 = tree.insert(List(0, 1, 2, 3))
      tree2 shouldBe Tree(0, Tree(1, Tree(2, Tree(3))))
      tree2.size shouldBe 4
      tree2.leafsSize shouldBe 1
    }

    "insert new branch to a single node Tree" in {
      val tree = Tree(0)
      tree.insert(List(0, 1, 2, 3)) shouldBe Tree(0, Tree(1, Tree(2, Tree(3))))
    }

    "insert new branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.size shouldBe 4
      tree.leafsSize shouldBe 1

      val tree2 = tree.insert(List(0, 1, 22, 33))
      tree2 shouldBe Tree(0, Tree(1, Tree(22, Tree(33)), Tree(2, Tree(3))))
      tree2.size shouldBe 6
      tree2.leafsSize shouldBe 2

      val tree3 = tree
        .insert(List(0, 1, 22, 33))
        .insert(List(0, 11, 12, 13))
      tree3 shouldBe Tree(0, Tree(11, Tree(12, Tree(13))), Tree(1, Tree(22, Tree(33)), Tree(2, Tree(3))))
      tree3.size shouldBe 9
      tree3.leafsSize shouldBe 3
    }

    "insert existing node to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(0)) shouldBe tree
    }

    "insert existing branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(0, 1, 2)) shouldBe tree
    }

    "try insert non-matching branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(7, 0)) shouldBe tree
    }

    "select an existing subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.select(List(0, 1)) shouldBe Some(Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.contains(List(0, 1)) shouldBe true
    }

    "try selecting non-existent subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.select(List(1, 2)) shouldBe None
      tree.contains(List(1, 2)) shouldBe false
    }

    "list all nodes" in {
      val tree = Tree.empty
      tree.nodesUnsafe shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodesUnsafe shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodesUnsafe shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "list all nodes using tail safe method" in {
      val tree = Tree.empty
      tree.nodes shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodes shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodes shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "stream all nodes" in {
      val tree = Tree.empty
      tree.nodeStream.toList shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodeStream.toList shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodeStream.toList shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "stream filtered nodes" in {
      val tree1 = Tree(0)
      tree1.nodeStream(_ > 15).toList shouldBe Nil
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodeStream(_ > 15).toList shouldBe List(20, 30, 21, 31, 22, 32)
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
      branches.forall(tree.contains) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insert(_)) shouldBe tree
    }

    "list all branches using tail safe method" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin

      val branches: List[List[Int]] = tree.branches()

      val graph = branches.map(_.mkString(" > ")).mkString("\n")
      graph should be(expected)
      graph shouldBe showAsGraph(tree)
      branches.forall(tree.contains) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insert(_)) shouldBe tree
    }

    "stream all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val graph = tree.branchStream.map(_.mkString(" > ")).mkString("\n")
      graph shouldBe """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin
      graph shouldBe showAsGraph(tree)
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

    "map all nodes" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val tree2 = tree.map(_ + 1)
      showAsGraph(tree2) shouldBe """1 > 12 > 21 > 31
                                    |1 > 13 > 22 > 32
                                    |1 > 13 > 23 > 33""".stripMargin
      val tree3 = tree.mapUnsafe(_ + 1)
      tree3 shouldBe tree2
    }

    "flatMap all nodes" in {
      val tree = Tree(0)
      val result = tree.flatMap(n => Tree(n + 1))
      showAsGraph(result) shouldBe "1"

      val tree2 = Tree(0, Tree(1))
      val result2 = tree2.flatMap(n => Tree(n + 1, Tree(n + 2)))
      showAsGraph(result2) shouldBe
        """1 > 2 > 3
          |1 > 2""".stripMargin

      val tree3 = Tree(0, Tree(5, Tree(10)))
      val result3 = tree3.flatMap(n => Tree(n + 1, Tree(n + 2, Tree(n + 3))))
      val graph3 = showAsGraph(result3)
      graph3 shouldBe
        """1 > 6 > 11 > 12 > 13
          |1 > 6 > 7 > 8
          |1 > 2 > 3""".stripMargin
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
      Tree().toValueList shouldBe Nil
      Tree("a").toValueList shouldBe List((0, "a"))
      Tree("a", Tree("b")).toValueList shouldBe List((0, "b"), (1, "a"))
      Tree("a", Tree("b1"), Tree("b2")).toValueList shouldBe List((0, "b2"), (0, "b1"), (2, "a"))
      Tree("a", Tree("b", Tree("c"))).toValueList shouldBe List((0, "c"), (1, "b"), (1, "a"))

      val tree1 = Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2"))))
      val tree1List = tree1.toValueList
      tree1List shouldBe List((0, "d2"), (1, "c2"), (1, "b2"), (0, "c1"), (1, "b1"), (2, "a"))
      Tree.Builder.fromValueList(tree1List) shouldBe List(tree1)

      val tree2 = Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2"))), Tree("b3"))
      val tree2List = tree2.toValueList
      tree2List shouldBe List((0, "b3"), (0, "d2"), (1, "c2"), (1, "b2"), (0, "c1"), (1, "b1"), (3, "a"))
      Tree.Builder.fromValueList(tree2List) shouldBe List(tree2)
    }

    "serialize a tree to a list of (numberOfChildren, subtree) pairs" in {
      Tree().toTreeList shouldBe Nil
      Tree("a").toTreeList shouldBe List((0, Tree("a")))
      Tree("a", Tree("b")).toTreeList shouldBe List((0, Tree("b")), (1, Tree("a")))
      Tree("a", Tree("b1"), Tree("b2")).toTreeList shouldBe List((0, Tree("b2")), (0, Tree("b1")), (2, Tree("a")))
      Tree("a", Tree("b", Tree("c"))).toTreeList shouldBe List((0, Tree("c")), (1, Tree("b")), (1, Tree("a")))

      val tree1 = Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2"))))
      val tree1List = tree1.toTreeList
      tree1List shouldBe List(
        (0, Tree("d2")),
        (1, Tree("c2")),
        (1, Tree("b2")),
        (0, Tree("c1")),
        (1, Tree("b1")),
        (2, Tree("a"))
      )
      Tree.Builder.fromTreeList(tree1List) shouldBe List(tree1)

      val tree2 = Tree("a", Tree("b1", Tree("c1")), Tree("b2", Tree("c2", Tree("d2"))), Tree("b3"))
      val tree2List = tree2.toTreeList
      tree2List shouldBe List(
        (0, Tree("b3")),
        (0, Tree("d2")),
        (1, Tree("c2")),
        (1, Tree("b2")),
        (0, Tree("c1")),
        (1, Tree("b1")),
        (3, Tree("a"))
      )
      Tree.Builder.fromTreeList(tree2List) shouldBe List(tree2)
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
  }

  "Tree.Builder" should {
    "create a new tree from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (0, "b"), (0, "c"), (3, "d"))

      val trees = Tree.Builder.fromValueList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.leafsSize shouldBe 3
      trees.head shouldBe Tree("d", Tree("c"), Tree("b"), Tree("a"))
      showAsGraph(trees.head) shouldBe
        """d > c
          |d > b
          |d > a""".stripMargin
    }

    "create a new trees from the list of values" in {
      val list: List[(Int, String)] = List((0, "a"), (1, "b"), (0, "c"), (1, "d"))

      val trees = Tree.Builder.fromValueList(list)

      trees.size shouldBe 2
      trees(0) shouldBe Tree("d", Tree("c"))
      trees(1) shouldBe Tree("b", Tree("a"))
    }

    "create a new tree from the list of single-node trees" in {
      val list: List[(Int, Tree[String])] = List((0, Tree("a")), (0, Tree("b")), (0, Tree("c")), (3, Tree("d")))

      val trees = Tree.Builder.fromTreeList(list)

      trees.size shouldBe 1
      trees.head.size shouldBe 4
      trees.head.leafsSize shouldBe 3
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
      trees.head.leafsSize shouldBe 4
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
      trees.head.leafsSize shouldBe 4
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
      trees.head.leafsSize shouldBe 1
      trees.head shouldBe Tree("d", Tree("D"))
      showAsGraph(trees.head) shouldBe "d > D"
    }
  }

}
