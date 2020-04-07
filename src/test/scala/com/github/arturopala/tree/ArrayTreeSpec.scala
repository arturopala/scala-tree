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

import com.github.arturopala.tree.util.ArrayTree._
import com.github.arturopala.tree.util.{Buffer, IntBuffer, IntSlice, Slice}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArrayTreeSpec extends AnyWordSpec with Matchers {

  val aa_a = Slice("aa", "a")
  val aaa_aa_a = Slice("aaa", "aa", "a")
  val a_aaa_aa_a = Slice("a", "aaa", "aa", "a")
  val aa_aaa_aa_a = Slice("aa", "aaa", "aa", "a")
  val aa_a_aaa_aa_a = Slice("aa", "a", "aaa", "aa", "a")

  "ArrayTree" should {
    "list children indexes" in {
      childrenIndexes(0, Array(0)) shouldBe Nil
      childrenIndexes(1, Array(0, 1)) shouldBe List(0)
      childrenIndexes(2, Array(0, 1, 1)) shouldBe List(1)
      childrenIndexes(3, Array(0, 0, 0, 3)) shouldBe List(2, 1, 0)
      childrenIndexes(4, Array(0, 1, 0, 1, 2)) shouldBe List(3, 1)
      childrenIndexes(3, Array(0, 1, 0, 1, 2)) shouldBe List(2)
      childrenIndexes(1, Array(0, 1, 0, 1, 2)) shouldBe List(0)
      childrenIndexes(6, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe List(5, 2)
      childrenIndexes(2, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe List(1, 0)
      childrenIndexes(5, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe List(4, 3)
      childrenIndexes(0, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe Nil
      childrenIndexes(1, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe Nil
      childrenIndexes(2, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe Nil
      childrenIndexes(3, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe Nil
      childrenIndexes(4, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe List(3, 2)
      childrenIndexes(5, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe List(4, 1)
      childrenIndexes(6, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe List(5, 0)
    }

    "append children indexes to buffer" in {
      val buffer = new IntBuffer()
      writeChildrenIndexes(0, Array(0), buffer, 0) shouldBe 0
      writeChildrenIndexes(1, Array(0, 1), buffer, 0) shouldBe 1
      buffer(0) shouldBe 0
      writeChildrenIndexes(2, Array(0, 0, 2), buffer, 1) shouldBe 2
      buffer(1) shouldBe 0
      buffer(2) shouldBe 1
      writeChildrenIndexes(3, Array(0, 0, 2, 1), buffer, 3) shouldBe 1
      buffer(3) shouldBe 2
      writeChildrenIndexes(4, Array(0, 0, 0, 0, 3, 2), buffer, 4) shouldBe 3
      buffer(4) shouldBe 1
      buffer(5) shouldBe 2
      buffer(6) shouldBe 3
    }

    "find rightmost index of children's node holding a value" in {
      childrenLeftmostIndexFor(2, -1, Array.empty[Int], Array.empty[Int]) shouldBe None
      childrenLeftmostIndexFor(2, 0, Array(0), Array(2)) shouldBe None
      childrenLeftmostIndexFor(2, 1, Array(0, 1), Array(2, 1)) shouldBe Some(0)
      childrenLeftmostIndexFor(2, 3, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(2)
      childrenLeftmostIndexFor(3, 3, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenLeftmostIndexFor(3, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(1)
      childrenLeftmostIndexFor(4, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(0)
      childrenLeftmostIndexFor(3, 1, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenLeftmostIndexFor(1, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenLeftmostIndexFor(1, 2, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe Some(1)
    }

    "find leftmost index of children's node holding a value" in {
      childrenRightmostIndexFor(2, -1, Array.empty[Int], Array.empty[Int]) shouldBe None
      childrenRightmostIndexFor(2, 0, Array(0), Array(2)) shouldBe None
      childrenRightmostIndexFor(2, 1, Array(0, 1), Array(2, 1)) shouldBe Some(0)
      childrenRightmostIndexFor(2, 3, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(2)
      childrenRightmostIndexFor(3, 3, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenRightmostIndexFor(3, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(1)
      childrenRightmostIndexFor(4, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(0)
      childrenRightmostIndexFor(3, 1, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenRightmostIndexFor(1, 2, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      childrenRightmostIndexFor(1, 2, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe Some(0)
    }

    "list indexes of children's nodes holding a value" in {
      childrenIndexListFor(1, 2, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe List(0, 1)
    }

    "find parent index" in {
      parentIndex(0, 2, Array(0, 1)) shouldBe 1
      parentIndex(0, 3, Array(0, 0, 2)) shouldBe 2
      parentIndex(1, 3, Array(0, 0, 2)) shouldBe 2
      parentIndex(0, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(1, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(2, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(0, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 6
      parentIndex(1, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 5
      parentIndex(2, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 4
      parentIndex(3, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 4
      parentIndex(4, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 5
      parentIndex(5, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe 6
      parentIndex(6, 7, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe -1
      parentIndex(0, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 2
      parentIndex(1, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 2
      parentIndex(2, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 6
      parentIndex(3, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 5
      parentIndex(4, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 5
      parentIndex(5, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe 6
      parentIndex(6, 7, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe -1
    }

    "iterate over tree's node indexes depth-first" in {
      nodeIndexIterator(0, Array(0)).toList shouldBe List(0)
      nodeIndexIterator(1, Array(0, 1)).toList shouldBe List(1, 0)
      nodeIndexIterator(2, Array(0, 1, 1)).toList shouldBe List(2, 1, 0)
      nodeIndexIterator(2, Array(0, 0, 2)).toList shouldBe List(2, 1, 0)
      nodeIndexIterator(3, Array(0, 0, 1, 2)).toList shouldBe List(3, 2, 1, 0)
      nodeIndexIterator(3, Array(0, 1, 0, 2)).toList shouldBe List(3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 0, 0, 0, 4)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 1, 0, 0, 3)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 0, 1, 0, 3)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 0, 0, 1, 3)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 0, 0, 2, 2)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(4, Array(0, 0, 2, 0, 2)).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIterator(3, Array(0, 1, 0, 1, 2)).toList shouldBe List(3, 2)
      nodeIndexIterator(2, Array(0, 1, 0, 1, 2)).toList shouldBe List(2)
      nodeIndexIterator(1, Array(0, 1, 0, 1, 2)).toList shouldBe List(1, 0)
      nodeIndexIterator(-1, Array.empty[Int]).toList shouldBe List()
    }

    "calculate subtree size" in {
      treeSize(0, Array(0)) shouldBe 1
      treeSize(0, Array(0, 1)) shouldBe 1
      treeSize(1, Array(0, 1)) shouldBe 2
      treeSize(2, Array(0, 1, 1)) shouldBe 3
      treeSize(2, Array(0, 0, 2)) shouldBe 3
      treeSize(3, Array(0, 0, 0, 3)) shouldBe 4
      treeSize(3, Array(0, 0, 1, 2)) shouldBe 4
      treeSize(3, Array(0, 0, 2, 1)) shouldBe 4
      treeSize(3, Array(0, 1, 0, 2)) shouldBe 4
      treeSize(4, Array(0, 0, 0, 0, 4)) shouldBe 5
      treeSize(4, Array(0, 0, 0, 2, 2)) shouldBe 5
      treeSize(4, Array(0, 1, 0, 1, 2)) shouldBe 5
      treeSize(4, Array(0, 0, 2, 0, 2)) shouldBe 5
      treeSize(4, Array(0, 1, 1, 0, 2)) shouldBe 5
      treeSize(4, Array(0, 0, 1, 0, 3)) shouldBe 5
      treeSize(2, Array(0, 0, 1, 0, 3)) shouldBe 2
      treeSize(3, Array(0, 0, 1, 0, 3)) shouldBe 1
      treeSize(1, Array(0, 0, 1, 0, 3)) shouldBe 1
    }

    "calculate malformed subtree size" in {
      an[IllegalArgumentException] shouldBe thrownBy(treeSize(0, Array(-1)))
      an[IllegalArgumentException] shouldBe thrownBy(treeSize(0, Array(1)))
      an[IllegalArgumentException] shouldBe thrownBy(treeSize(0, Array(2)))
      an[IllegalArgumentException] shouldBe thrownBy(treeSize(1, Array(0, 2)))
      an[IllegalArgumentException] shouldBe thrownBy(treeSize(2, Array(0, 2, 1)))
      treeSize(1, Array(1, 0)) shouldBe 1
      treeSize(2, Array(0, 0, 1)) shouldBe 2
    }

    "iterate over tree's branches as index lists" in {
      branchesIndexListIterator(0, Array(0)).map(_.toList).toList shouldBe List(List(0))
      branchesIndexListIterator(1, Array(0, 1)).map(_.toList).toList shouldBe List(List(1, 0))
      branchesIndexListIterator(2, Array(0, 1, 1)).map(_.toList).toList shouldBe List(List(2, 1, 0))
      branchesIndexListIterator(2, Array(0, 0, 2)).map(_.toList).toList shouldBe List(List(2, 1), List(2, 0))
      branchesIndexListIterator(3, Array(0, 0, 0, 3)).map(_.toList).toList shouldBe List(
        List(3, 2),
        List(3, 1),
        List(3, 0)
      )
      branchesIndexListIterator(3, Array(0, 0, 2, 1)).map(_.toList).toList shouldBe List(List(3, 2, 1), List(3, 2, 0))
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4)).map(_.toList).toList shouldBe List(
        List(9, 8, 7),
        List(9, 8, 6),
        List(9, 5, 4, 3),
        List(9, 2),
        List(9, 1, 0)
      )
    }

    "iterate over tree's branches as values lists without filter" in {
      val v: Int => Int = _ * 10
      val f: Iterable[Int] => Boolean = _ => true
      branchIterator(0, Array(0), v, f).map(_.toList).toList shouldBe List(List(0))
      branchIterator(1, Array(0, 1), v, f).map(_.toList).toList shouldBe List(List(10, 0))
      branchIterator(2, Array(0, 1, 1), v, f).map(_.toList).toList shouldBe List(List(20, 10, 0))
      branchIterator(2, Array(0, 0, 2), v, f).map(_.toList).toList shouldBe List(List(20, 10), List(20, 0))
      branchIterator(3, Array(0, 0, 0, 3), v, f).map(_.toList).toList shouldBe List(
        List(30, 20),
        List(30, 10),
        List(30, 0)
      )
      branchIterator(3, Array(0, 0, 2, 1), v, f).map(_.toList).toList shouldBe List(List(30, 20, 10), List(30, 20, 0))
    }

    "fold branches as index lists" in {
      val fold =
        (s: (Int, Int, Int), a: IntSlice, _: Int) => (s._1 + 1, Math.max(s._2, a.length), s._3 + a.length)
      foldLeftBranchesIndexLists(-1, Array.empty[Int], (0, 0, 0), fold) shouldBe (0, 0, 0)
      foldLeftBranchesIndexLists(0, Array(0), (0, 0, 0), fold) shouldBe (1, 1, 1)
      foldLeftBranchesIndexLists(1, Array(0, 1), (0, 0, 0), fold) shouldBe (1, 2, 2)
      foldLeftBranchesIndexLists(2, Array(0, 1, 1), (0, 0, 0), fold) shouldBe (1, 3, 3)
      foldLeftBranchesIndexLists(2, Array(0, 0, 2), (0, 0, 0), fold) shouldBe (2, 2, 4)
      foldLeftBranchesIndexLists(4, Array(0, 1, 0, 1, 2), (0, 0, 0), fold) shouldBe (2, 3, 6)
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 1, 3), (0, 0, 0), fold) shouldBe (3, 3, 7)
      foldLeftBranchesIndexLists(4, Array(0, 1, 1, 1, 1), (0, 0, 0), fold) shouldBe (1, 5, 5)
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 2, 2), (0, 0, 0), fold) shouldBe (3, 3, 8)
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 0, 4), (0, 0, 0), fold) shouldBe (4, 2, 8)
    }

    "fold branches lengths" in {
      val fold = (s: (Int, Int, Int), l: Int, _: Int) => (s._1 + 1, Math.max(s._2, l), s._3 + l)
      foldLeftBranchesLengths(-1, Array.empty[Int], (0, 0, 0), fold) shouldBe (0, 0, 0)
      foldLeftBranchesLengths(0, Array(0), (0, 0, 0), fold) shouldBe (1, 1, 1)
      foldLeftBranchesLengths(1, Array(0, 1), (0, 0, 0), fold) shouldBe (1, 2, 2)
      foldLeftBranchesLengths(2, Array(0, 1, 1), (0, 0, 0), fold) shouldBe (1, 3, 3)
      foldLeftBranchesLengths(2, Array(0, 0, 2), (0, 0, 0), fold) shouldBe (2, 2, 4)
      foldLeftBranchesLengths(4, Array(0, 1, 0, 1, 2), (0, 0, 0), fold) shouldBe (2, 3, 6)
      foldLeftBranchesLengths(4, Array(0, 0, 0, 1, 3), (0, 0, 0), fold) shouldBe (3, 3, 7)
      foldLeftBranchesLengths(4, Array(0, 1, 1, 1, 1), (0, 0, 0), fold) shouldBe (1, 5, 5)
      foldLeftBranchesLengths(4, Array(0, 0, 0, 2, 2), (0, 0, 0), fold) shouldBe (3, 3, 8)
      foldLeftBranchesLengths(4, Array(0, 0, 0, 0, 4), (0, 0, 0), fold) shouldBe (4, 2, 8)
    }

    "calculate height of the tree" in {
      calculateHeight(-1, Array.empty[Int]) shouldBe 0
      calculateHeight(0, Array(0)) shouldBe 1
      calculateHeight(0, Slice(0)) shouldBe 1
      calculateHeight(1, Array(0, 1)) shouldBe 2
      calculateHeight(1, Slice(0, 1)) shouldBe 2
      calculateHeight(2, Array(0, 1, 1)) shouldBe 3
      calculateHeight(2, Array(0, 0, 2)) shouldBe 2
      calculateHeight(4, Array(0, 1, 0, 1, 2)) shouldBe 3
      calculateHeight(4, Array(0, 0, 0, 1, 3)) shouldBe 3
      calculateHeight(4, Array(0, 1, 1, 1, 1)) shouldBe 5
      calculateHeight(4, Array(0, 0, 0, 2, 2)) shouldBe 3
      calculateHeight(4, Array(0, 0, 0, 0, 4)) shouldBe 2
      calculateHeight(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4)) shouldBe 4
    }

    "calculate height of an incomplete tree" in {
      calculateHeight(0, Array(1)) shouldBe 2
      calculateHeight(1, Array(1, 1)) shouldBe 3
    }

    "access a tree at the index" in {
      treeAt(0, IntSlice(0), Slice("a")).height shouldBe 1
      treeAt(1, IntSlice(0, 1), Slice("a", "b")).height shouldBe 2
      treeAt(0, IntSlice(0, 1), Slice("a", "b")).height shouldBe 1
      treeAt(2, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 3
      treeAt(1, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 2
      treeAt(0, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 1
    }

    "iterate over tree's subtrees" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treeIterator(0, IntSlice(0), Slice("a"), f).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treeIterator(1, IntSlice(0, 1), Slice("b", "a"), f).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treeIterator(2, IntSlice(0, 1, 1), Slice("c", "b", "a"), f).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c")),
        Tree("c")
      )
      treeIterator(1, IntSlice(0, 1, 1), Slice("c", "b", "a"), f).toList shouldBe List(Tree("b", Tree("c")), Tree("c"))
      treeIterator(2, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treeIterator(1, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(Tree("b"))
      treeIterator(0, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(Tree("c"))
      treeIterator(2, IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a"), f).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "count branches" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _ => true
      countBranches[String](-1, Array.empty[Int], Array.empty[String], f) shouldBe 0
      countBranches[String](0, Array(0), v, f) shouldBe 1
      countBranches[String](0, Array(0, 1), v, f) shouldBe 1
      countBranches[String](1, Array(0, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 1, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2), v, f) shouldBe 2
      countBranches[String](3, Array(0, 0, 0, 3), v, f) shouldBe 3
      countBranches[String](3, Array(0, 0, 1, 2), v, f) shouldBe 2
      countBranches[String](2, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 2
      countBranches[String](7, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 4
      countBranches[String](9, Array(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), v, f) shouldBe 5
    }

    "count branches fulfilling the predicate" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _.size > 2
      countBranches[String](-1, Array.empty[Int], Array.empty[String], f) shouldBe 0
      countBranches[String](0, Array(0), v, f) shouldBe 0
      countBranches[String](0, Array(0, 1), v, f) shouldBe 0
      countBranches[String](1, Array(0, 1), v, f) shouldBe 0
      countBranches[String](2, Array(0, 1, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2), v, f) shouldBe 0
      countBranches[String](3, Array(0, 0, 0, 3), v, f) shouldBe 0
      countBranches[String](3, Array(0, 0, 1, 2), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 0
      countBranches[String](7, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 4
      countBranches[String](9, Array(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), v, f) shouldBe 4
    }

    "follow the path into tree" in {
      def see[T](t: (Array[Int], Option[String], Iterator[T], Boolean)): (List[Int], Option[String], List[T], Boolean) =
        (t._1.toList, t._2, t._3.toList, t._4)

      see(followPath(List("a"), -1, Array.empty[Int], Array.empty[String])) shouldBe (Nil, Some("a"), Nil, false)
      see(followPath(List("a"), 0, Array(0), Array("a"))) shouldBe (List(0), None, Nil, true)
      see(followPath(List("a", "b"), 1, Array(0, 1), Array("b", "a"))) shouldBe (List(1, 0), None, Nil, true)
      see(followPath(List("a", "b"), 1, Array(0, 1), Array("c", "a"))) shouldBe (List(1), Some("b"), Nil, false)
      see(followPath(List("a", "b", "c"), 2, Array(0, 1, 1), Array("c", "b", "a"))) shouldBe (List(2, 1, 0), None, Nil, true)
      see(followPath(List("a", "b"), 2, Array(0, 1, 1), Array("c", "b", "a"))) shouldBe (List(2, 1), None, Nil, false)
      see(followPath(List("a", "c"), 2, Array(0, 1, 1), Array("c", "b", "a"))) shouldBe (List(2), Some("c"), Nil, false)
      see(followPath(List("a", "c", "d"), 2, Array(0, 1, 1), Array("c", "b", "a"))) shouldBe (List(2), Some("c"), List(
        "d"
      ), false)
      see(followPath(List("a", "c", "d"), 1, Array(0, 1, 1), Array("c", "b", "a"))) shouldBe (Nil, Some("a"), List(
        "c",
        "d"
      ), false)
      see(followPath(List("a", "b"), 2, Array(0, 0, 2), Array("c", "b", "a"))) shouldBe (List(2, 1), None, Nil, true)
      see(followPath(List("a", "c"), 2, Array(0, 0, 2), Array("c", "b", "a"))) shouldBe (List(2, 0), None, Nil, true)

      val v4 = Array("d", "c", "b", "a")
      val s4_1 = Array(0, 0, 1, 2)
      see(followPath(List("a", "d"), 3, s4_1, v4)) shouldBe (List(3, 0), None, Nil, true)
      see(followPath(List("a", "b"), 3, s4_1, v4)) shouldBe (List(3, 2), None, Nil, false)
      see(followPath(List("a", "b", "c"), 3, s4_1, v4)) shouldBe (List(3, 2, 1), None, Nil, true)
      see(followPath(List("a", "b", "d"), 3, s4_1, v4)) shouldBe (List(3, 2), Some("d"), Nil, false)

      val v7 = Array("g", "f", "e", "d", "c", "b", "a")
      val s7_1 = Array(0, 0, 2, 0, 0, 2, 2)
      see(followPath(List("a", "e", "g"), 6, s7_1, v7)) shouldBe (List(6, 2, 0), None, Nil, true)
      see(followPath(List("a", "b", "d"), 6, s7_1, v7)) shouldBe (List(6, 5, 3), None, Nil, true)
      see(followPath(List("a", "b", "e"), 6, s7_1, v7)) shouldBe (List(6, 5), Some("e"), Nil, false)

      val s7_2 = Array(0, 1, 1, 0, 1, 2, 1)
      see(followPath(List("a", "e", "g"), 6, s7_2, v7)) shouldBe (List(6), Some("e"), List("g"), false)
      see(followPath(List("a", "b", "c"), 6, s7_2, v7)) shouldBe (List(6, 5, 4), None, Nil, false)
      see(followPath(List("a", "b", "d"), 6, s7_2, v7)) shouldBe (List(6, 5), Some("d"), Nil, false)
      see(followPath(List("a", "b", "e"), 6, s7_2, v7)) shouldBe (List(6, 5, 2), None, Nil, false)
      see(followPath(List("a", "b", "e", "f"), 6, s7_2, v7)) shouldBe (List(6, 5, 2, 1), None, Nil, false)
      see(followPath(List("a", "b", "e", "f", "g"), 6, s7_2, v7)) shouldBe (List(6, 5, 2, 1, 0), None, Nil, true)
    }

    "flatMap a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMapDistinct(IntSlice(), Slice.empty[String], f0) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f0) shouldBe Tree(1)
      flatMapDistinct(IntSlice(0, 1), aa_a, f0) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1, Tree(2))))
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMap(IntSlice(), Slice.empty[String], f1) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f1) shouldBe Tree(1, Tree(2))
      flatMap(IntSlice(0, 1), aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(2))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(2)
      )
      flatMap(IntSlice(0, 0, 2), aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(2)
      )

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMap(IntSlice(), Slice.empty[String], f2) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f2) shouldBe Tree(1, Tree(2), Tree(2))
      flatMap(IntSlice(0, 1), aa_a, f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)), Tree(2), Tree(2))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4), Tree(3)),
        Tree(2),
        Tree(2)
      )

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMap(IntSlice(), Slice.empty[String], f3) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f3) shouldBe Tree.empty
      flatMap(IntSlice(0, 1), aa_a, f3) shouldBe Tree.empty
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMap(IntSlice(), Slice.empty[String], f4) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f4) shouldBe Tree.empty
      flatMap(IntSlice(0, 1), aa_a, f4) shouldBe Tree(2, Tree(4))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))

      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0), Slice("a"), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0, 1), Slice("b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0, 0, 2), Slice("c", "b", "a"), f5) shouldBe
        Tree(
          0,
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(1, Tree(2, Tree(4, Tree(5))))
        )
    }

    "debug" in {}

    "flatMap distinct a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMapDistinct(IntSlice(), Slice.empty[String], f0) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f0) shouldBe Tree(1)
      flatMapDistinct(IntSlice(0, 1), aa_a, f0) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1, Tree(2))))
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(IntSlice(), Slice.empty[String], f1) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f1) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1), aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f1) shouldBe Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(3, Tree(6)))
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1, Tree(2)), Tree(6)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(1, Tree(2)), Tree(6))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2, Tree(4))), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f1) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)), Tree(1, Tree(2, Tree(4))))

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMapDistinct(IntSlice(), Slice.empty[String], f2) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f2) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1), aa_a, f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f2) shouldBe Tree(1, Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(6), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), aa_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(3, Tree(6), Tree(4)),
        Tree(2, Tree(4), Tree(3))
      )
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(1, Tree(2)), Tree(6), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f2) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))))

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMapDistinct(IntSlice(), Slice.empty[String], f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1), aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(IntSlice(), Slice.empty[String], f4) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f4) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1), aa_a, f4) shouldBe Tree(2, Tree(4))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f4) shouldBe Tree.empty

      // test direct subtree insertion
      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0), Slice("a"), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0, 1), Slice("b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0, 0, 2), Slice("c", "b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
    }

    "insert a value" in {
      insertValue(0, "a", Tree.empty) shouldBe Tree("a")
      insertValue(0, "a", Tree("a")) shouldBe Tree("a", Tree("a"))
      insertValue(1, "a", Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertValue(0, "a", Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertValue(2, "a", Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      insertValue(1, "a", Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      insertValue(0, "a", Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      insertValue(0, "a", Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      insertValue(1, "a", Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      insertValue(2, "a", Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("a"), Tree("b", Tree("c")))
    }

    "insert a subtree" in {
      insertSubtree(0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertSubtree(0, Tree("a"), Tree.empty) shouldBe Tree("a")
      insertSubtree(0, Tree.empty, Tree("a")) shouldBe Tree("a")
      insertSubtree(0, Tree("a"), Tree("a")) shouldBe Tree("a", Tree("a"))
      insertSubtree(1, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertSubtree(0, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertSubtree(1, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")), Tree("b"))
      insertSubtree(0, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("b", Tree("a"))))
      insertSubtree(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b")))) shouldBe Tree(
        "b",
        Tree("a", Tree("b")),
        Tree("a", Tree("b"))
      )
    }

    "insert a subtree distinct" in {
      insertSubtreeDistinct(0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertSubtreeDistinct(0, Tree("a"), Tree.empty) shouldBe Tree("a")
      insertSubtreeDistinct(0, Tree.empty, Tree("a")) shouldBe Tree("a")
      insertSubtreeDistinct(0, Tree("a"), Tree("a")) shouldBe Tree("a", Tree("a"))
      insertSubtreeDistinct(1, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertSubtreeDistinct(0, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertSubtreeDistinct(1, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertSubtreeDistinct(0, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree(
        "a",
        Tree("b", Tree("b", Tree("a")))
      )
      insertSubtreeDistinct(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b")))) shouldBe Tree(
        "b",
        Tree("a", Tree("b"))
      )
    }

    "insert a branch into the buffers" in {
      insertBranch(List.empty[String].iterator, 0, IntBuffer.empty, Buffer.empty[String], 0) shouldBe 0
      insertBranch(List("a").iterator, -1, IntBuffer.empty, Buffer.empty[String], 0) shouldBe 1
      insertBranch(List("a", "b").iterator, -1, IntBuffer.empty, Buffer.empty[String], 0) shouldBe 2
      insertBranch(List("b").iterator, 0, IntBuffer(0), Buffer("a"), 0) shouldBe 1
      insertBranch(List("a").iterator, 0, IntBuffer(0), Buffer("a"), 0) shouldBe 1
      insertBranch(List("a", "b", "c").iterator, 0, IntBuffer(0), Buffer("a"), 0) shouldBe 3
      insertBranch(List("b", "c").iterator, 1, IntBuffer(0, 1), Buffer("a", "b"), 0) shouldBe 2
      insertBranch(List("b", "c").iterator, 1, IntBuffer(0, 1), Buffer("b", "a"), 0) shouldBe 1
      insertBranch(List("b", "c", "d", "e", "f").iterator, 1, IntBuffer(0, 1), Buffer("b", "a"), 0) shouldBe 4
      insertBranch(List("b", "c", "d", "e", "f").iterator, 2, IntBuffer(0, 1, 1), Buffer("c", "b", "a"), 0) shouldBe 3
      insertBranch(List("b", "c", "d", "e", "f").iterator, 2, IntBuffer(0, 0, 2), Buffer("c", "b", "a"), 0) shouldBe 4
    }

    "insert a branch into the tree" in {
      insertBranch(-1, List(), Tree.empty) shouldBe Tree.empty
      insertBranch(-1, List("a"), Tree.empty) shouldBe Tree("a")
      insertBranch(-1, List("a", "b", "c", "d", "e"), Tree.empty) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d", Tree("e"))))
      )
      insertBranch(0, List("a"), Tree("a")) shouldBe Tree("a")
      insertBranch(1, List("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "b"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "c"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("c"), Tree("b"))
      insertBranch(3, List("a", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "c", "e"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("e")),
        Tree("d")
      )
      insertBranch(3, List("a", "b", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "d", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d", Tree("c"))
      )
      insertBranch(3, List("a", "e", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("e", Tree("c")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
    }
  }

}
