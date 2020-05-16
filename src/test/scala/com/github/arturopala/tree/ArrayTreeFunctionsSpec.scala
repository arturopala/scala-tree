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

import com.github.arturopala.tree.internal.ArrayTreeFunctions._
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

class ArrayTreeFunctionsSpec extends AnyWordSpecCompat {

  "ArrayTreeFunctions" should {

    "list children indexes" in {
      childrenIndexes(0, Array(0)) shouldBe IntSlice.empty
      childrenIndexes(1, Array(0, 1)) shouldBe IntSlice(0)
      childrenIndexes(2, Array(0, 1, 1)) shouldBe IntSlice(1)
      childrenIndexes(3, Array(0, 0, 0, 3)) shouldBe IntSlice(2, 1, 0)
      childrenIndexes(4, Array(0, 1, 0, 1, 2)) shouldBe IntSlice(3, 1)
      childrenIndexes(3, Array(0, 1, 0, 1, 2)) shouldBe IntSlice(2)
      childrenIndexes(1, Array(0, 1, 0, 1, 2)) shouldBe IntSlice(0)
      childrenIndexes(6, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe IntSlice(5, 2)
      childrenIndexes(2, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe IntSlice(1, 0)
      childrenIndexes(5, Array(0, 0, 2, 0, 0, 2, 2)) shouldBe IntSlice(4, 3)
      childrenIndexes(0, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice.empty
      childrenIndexes(1, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice.empty
      childrenIndexes(2, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice.empty
      childrenIndexes(3, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice.empty
      childrenIndexes(4, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice(3, 2)
      childrenIndexes(5, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice(4, 1)
      childrenIndexes(6, Array(0, 0, 0, 0, 2, 2, 2)) shouldBe IntSlice(5, 0)
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

    "find leftmost index of child's node holding a value" in {
      leftmostIndexOfChildValue(2, -1, 0, Array.empty[Int], Array.empty[Int]) shouldBe None
      leftmostIndexOfChildValue(2, 0, 1, Array(0), Array(2)) shouldBe None
      leftmostIndexOfChildValue(2, 1, 2, Array(0, 1), Array(2, 1)) shouldBe Some(0)
      leftmostIndexOfChildValue(2, 3, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(2)
      leftmostIndexOfChildValue(3, 3, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      leftmostIndexOfChildValue(3, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(1)
      leftmostIndexOfChildValue(4, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(0)
      leftmostIndexOfChildValue(3, 1, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      leftmostIndexOfChildValue(1, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      leftmostIndexOfChildValue(1, 2, 4, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe Some(1)
    }

    "find rightmost index of child's node holding a value" in {
      rightmostIndexOfChildValue(2, -1, 0, Array.empty[Int], Array.empty[Int]) shouldBe None
      rightmostIndexOfChildValue(2, 0, 1, Array(0), Array(2)) shouldBe None
      rightmostIndexOfChildValue(2, 1, 2, Array(0, 1), Array(2, 1)) shouldBe Some(0)
      rightmostIndexOfChildValue(2, 3, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(2)
      rightmostIndexOfChildValue(3, 3, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      rightmostIndexOfChildValue(3, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(1)
      rightmostIndexOfChildValue(4, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe Some(0)
      rightmostIndexOfChildValue(3, 1, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      rightmostIndexOfChildValue(1, 2, 4, Array(0, 0, 2, 1), Array(4, 3, 2, 1)) shouldBe None
      rightmostIndexOfChildValue(1, 2, 4, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe Some(0)
    }

    "list indexes of children's nodes holding a value" in {
      childrenIndexesFor(1, 2, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe IntSlice(1, 0)
      childrenIndexesFor(1, 2, Array(0, 0, 2, 1), Array(1, 1, 1, 1)) shouldBe IntSlice(1, 0)
      childrenIndexesFor(2, 3, Array(0, 0, 0, 3), Array(1, 1, 1, 1)) shouldBe IntSlice()
      childrenIndexesFor(2, -1, Array(0, 0, 0, 3), Array(1, 1, 1, 1)) shouldBe IntSlice()
    }

    "find parent index" in {
      parentIndex(0, 2, Array(0, 1)) shouldBe 1
      parentIndex(1, 2, Array(0, 1)) shouldBe -1
      parentIndex(0, 3, Array(0, 0, 2)) shouldBe 2
      parentIndex(1, 3, Array(0, 0, 2)) shouldBe 2
      parentIndex(2, 3, Array(0, 0, 2)) shouldBe -1
      parentIndex(0, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(1, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(2, 4, Array(0, 0, 0, 3)) shouldBe 3
      parentIndex(3, 4, Array(0, 0, 0, 3)) shouldBe -1
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

    "iterate over tree's node indexes depth-first with depth limit" in {
      nodeIndexIteratorWithLimit(0, Array(0), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(0, Array(0), 1).toList shouldBe List(0)
      nodeIndexIteratorWithLimit(0, Array(0), 2).toList shouldBe List(0)
      nodeIndexIteratorWithLimit(1, Array(0, 1), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(1, Array(0, 1), 1).toList shouldBe List(1)
      nodeIndexIteratorWithLimit(1, Array(0, 1), 2).toList shouldBe List(1, 0)
      nodeIndexIteratorWithLimit(1, Array(0, 1), 3).toList shouldBe List(1, 0)
      nodeIndexIteratorWithLimit(2, Array(0, 1, 1), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(2, Array(0, 1, 1), 1).toList shouldBe List(2)
      nodeIndexIteratorWithLimit(2, Array(0, 1, 1), 2).toList shouldBe List(2, 1)
      nodeIndexIteratorWithLimit(2, Array(0, 1, 1), 3).toList shouldBe List(2, 1, 0)
      nodeIndexIteratorWithLimit(2, Array(0, 0, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(2, Array(0, 0, 2), 1).toList shouldBe List(2)
      nodeIndexIteratorWithLimit(2, Array(0, 0, 2), 2).toList shouldBe List(2, 1, 0)
      nodeIndexIteratorWithLimit(2, Array(0, 0, 2), 3).toList shouldBe List(2, 1, 0)
      nodeIndexIteratorWithLimit(3, Array(0, 0, 1, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(3, Array(0, 0, 1, 2), 1).toList shouldBe List(3)
      nodeIndexIteratorWithLimit(3, Array(0, 0, 1, 2), 2).toList shouldBe List(3, 2, 0)
      nodeIndexIteratorWithLimit(3, Array(0, 0, 1, 2), 3).toList shouldBe List(3, 2, 1, 0)
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 2), 1).toList shouldBe List(3)
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 2), 2).toList shouldBe List(3, 2, 1)
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 2), 3).toList shouldBe List(3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 0, 4), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 0, 4), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 0, 4), 2).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 0, 4), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 1, 0, 0, 3), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 1, 0, 0, 3), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 1, 0, 0, 3), 2).toList shouldBe List(4, 3, 2, 1)
      nodeIndexIteratorWithLimit(4, Array(0, 1, 0, 0, 3), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 1, 0, 3), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 0, 1, 0, 3), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 1, 0, 3), 2).toList shouldBe List(4, 3, 2, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 1, 0, 3), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 1, 3), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 1, 3), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 1, 3), 2).toList shouldBe List(4, 3, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 1, 3), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 2, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 2, 2), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 2, 2), 2).toList shouldBe List(4, 3, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 0, 2, 2), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 2, 0, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(4, Array(0, 0, 2, 0, 2), 1).toList shouldBe List(4)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 2, 0, 2), 2).toList shouldBe List(4, 3, 2)
      nodeIndexIteratorWithLimit(4, Array(0, 0, 2, 0, 2), 3).toList shouldBe List(4, 3, 2, 1, 0)
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 1, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 1, 2), 1).toList shouldBe List(3)
      nodeIndexIteratorWithLimit(3, Array(0, 1, 0, 1, 2), 2).toList shouldBe List(3, 2)
      nodeIndexIteratorWithLimit(2, Array(0, 1, 0, 1, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(2, Array(0, 1, 0, 1, 2), 1).toList shouldBe List(2)
      nodeIndexIteratorWithLimit(1, Array(0, 1, 0, 1, 2), 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(1, Array(0, 1, 0, 1, 2), 1).toList shouldBe List(1)
      nodeIndexIteratorWithLimit(1, Array(0, 1, 0, 1, 2), 2).toList shouldBe List(1, 0)
      nodeIndexIteratorWithLimit(-1, Array.empty[Int], 0).toList shouldBe Nil
      nodeIndexIteratorWithLimit(-1, Array.empty[Int], 1).toList shouldBe Nil
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

    "iterate over tree's branches as index lists with depth limit" in {
      branchesIndexListIterator(0, Array(0), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(0, Array(0), 1).map(_.toList).toList shouldBe List(List(0))
      branchesIndexListIterator(1, Array(0, 1), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(1, Array(0, 1), 1).map(_.toList).toList shouldBe List(List(1))
      branchesIndexListIterator(1, Array(0, 1), 2).map(_.toList).toList shouldBe List(List(1, 0))
      branchesIndexListIterator(1, Array(0, 1), 3).map(_.toList).toList shouldBe List(List(1, 0))
      branchesIndexListIterator(2, Array(0, 1, 1), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(2, Array(0, 1, 1), 1).map(_.toList).toList shouldBe List(List(2))
      branchesIndexListIterator(2, Array(0, 1, 1), 2).map(_.toList).toList shouldBe List(List(2, 1))
      branchesIndexListIterator(2, Array(0, 1, 1), 3).map(_.toList).toList shouldBe List(List(2, 1, 0))
      branchesIndexListIterator(2, Array(0, 1, 1), 4).map(_.toList).toList shouldBe List(List(2, 1, 0))
      branchesIndexListIterator(2, Array(0, 0, 2), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(2, Array(0, 0, 2), 1).map(_.toList).toList shouldBe List(List(2))
      branchesIndexListIterator(2, Array(0, 0, 2), 2).map(_.toList).toList shouldBe List(List(2, 1), List(2, 0))
      branchesIndexListIterator(2, Array(0, 0, 2), 3).map(_.toList).toList shouldBe List(List(2, 1), List(2, 0))
      branchesIndexListIterator(3, Array(0, 0, 0, 3), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(3, Array(0, 0, 0, 3), 1).map(_.toList).toList shouldBe List(
        List(3)
      )
      branchesIndexListIterator(3, Array(0, 0, 0, 3), 2).map(_.toList).toList shouldBe List(
        List(3, 2),
        List(3, 1),
        List(3, 0)
      )
      branchesIndexListIterator(3, Array(0, 0, 2, 1), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(3, Array(0, 0, 2, 1), 1).map(_.toList).toList shouldBe List(List(3))
      branchesIndexListIterator(3, Array(0, 0, 2, 1), 2).map(_.toList).toList shouldBe List(List(3, 2))
      branchesIndexListIterator(3, Array(0, 0, 2, 1), 3).map(_.toList).toList shouldBe List(
        List(3, 2, 1),
        List(3, 2, 0)
      )
      branchesIndexListIterator(3, Array(0, 0, 2, 1), 4).map(_.toList).toList shouldBe List(
        List(3, 2, 1),
        List(3, 2, 0)
      )
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4), 0).map(_.toList).toList shouldBe List()
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4), 1).map(_.toList).toList shouldBe List(List(9))
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4), 2).map(_.toList).toList shouldBe List(
        List(9, 8),
        List(9, 5),
        List(9, 2),
        List(9, 1)
      )
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4), 3).map(_.toList).toList shouldBe List(
        List(9, 8, 7),
        List(9, 8, 6),
        List(9, 5, 4),
        List(9, 2),
        List(9, 1, 0)
      )
      branchesIndexListIterator(9, Array(0, 1, 0, 0, 1, 1, 0, 0, 2, 4), 4).map(_.toList).toList shouldBe List(
        List(9, 8, 7),
        List(9, 8, 6),
        List(9, 5, 4, 3),
        List(9, 2),
        List(9, 1, 0)
      )
    }

    "fold branches as index lists with maxDepth set" in {
      val fold = (s: Int, a: IntSlice, _: Int) => Math.max(s, a.length)
      foldLeftBranchesIndexLists(-1, Array.empty[Int], 0, fold, 2) shouldBe 0
      foldLeftBranchesIndexLists(0, Array(0), 0, fold, 2) shouldBe 1
      foldLeftBranchesIndexLists(1, Array(0, 1), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(2, Array(0, 1, 1), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(2, Array(0, 0, 2), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(4, Array(0, 1, 0, 1, 2), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 1, 3), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(4, Array(0, 1, 1, 1, 1), 0, fold, 3) shouldBe 3
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 2, 2), 0, fold, 2) shouldBe 2
      foldLeftBranchesIndexLists(4, Array(0, 0, 0, 0, 4), 0, fold, 2) shouldBe 2
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

    "follow path into tree" in {
      def see[T](t: (IntSlice, Option[String], Iterator[T], Boolean)): (List[Int], Option[String], List[T], Boolean) =
        (t._1.toList, t._2, t._3.toList, t._4)

      see(followPath(List("a"), -1, Array.empty[Int], Array.empty[String])) shouldBe (Nil, Some("a"), Nil, false)
      see(followPath(List("a"), 0, Array(0), Array("a"))) shouldBe (List(0), None, Nil, true)
      see(followPath(List("a", "a"), 0, Array(0), Array("a"))) shouldBe (List(0), Some("a"), Nil, false)
      see(followPath(List("a", "b", "c"), 0, Array(0), Array("a"))) shouldBe (List(0), Some("b"), List("c"), false)
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

    def testWithBuffers[T, R](test: (IntBuffer, Buffer[T]) => R, structure: IntBuffer, values: Buffer[T])(
      assert: (Array[Int], Array[T], R) => Unit
    ): Unit = {
      val result = test(structure, values)
      assert(structure.toArray, values.toArray, result)
    }

    "expand value into a tree at index" in {
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0), Slice("a"), -1, _, _),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](expandValueIntoTree(IntSlice(0), Slice("b"), -1, _, _), IntBuffer(0), Buffer("a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice.empty, Slice.empty[String], 0, _, _),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0, 1), Slice("b", "a"), 0, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("b", "a", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0, 0, 2), Slice("c", "b", "a"), 0, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("c", "b", "a", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0, 0, 2), Slice("c", "e", "a"), 1, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3)
          values shouldBe Array("c", "e", "b", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0, 0, 2), Slice("f", "e", "d"), 1, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 2, 2)
          values shouldBe Array("c", "f", "e", "d", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTree(IntSlice(0, 0, 2), Slice("f", "e", "d"), 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 0, 4)
          values shouldBe Array("f", "e", "c", "b", "d")
          delta shouldBe 2
      }
    }

    "expand distinct value into a tree at index" in {
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0), Slice("a"), -1, -1, _, _),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0), Slice("b"), -1, 0, _, _),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice.empty, Slice.empty[String], 0, -1, _, _),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 1), Slice("b", "a"), 0, 1, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("b", "a", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 0, 2), Slice("c", "b", "a"), 0, 1, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("c", "b", "a", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 0, 2), Slice("c", "e", "a"), 1, -1, _, _),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3)
          values shouldBe Array("c", "e", "b", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 0, 2), Slice("f", "e", "d"), 1, 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 2, 2)
          values shouldBe Array("c", "f", "e", "d", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 0, 2), Slice("f", "e", "d"), 2, -1, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 0, 4)
          values shouldBe Array("f", "e", "c", "b", "d")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0), Slice("b"), 0, 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 1), Slice("d", "b"), 0, 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("d", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 1, 1, 1), Slice("f", "e", "d", "b"), 0, 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1, 1, 1)
          values shouldBe Array("f", "e", "d", "b", "a")
          delta shouldBe 2
      }
      testWithBuffers[String, Int](
        expandValueIntoTreeDistinct(IntSlice(0, 1, 1, 1), Slice("f", "e", "d", "c"), 1, 2, _, _),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1, 1, 1)
          values shouldBe Array("f", "e", "d", "c", "a")
          delta shouldBe 2
      }
    }

    "remove value at index" in {
      testWithBuffers[String, Int](removeValue(0, -1, _, _), IntBuffer.empty, Buffer.empty[String]) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](removeValue(0, -1, _, _), IntBuffer(0), Buffer("a")) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeValue(1, -1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("b")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeValue(0, 1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeValue(0, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeValue(1, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("c", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeValue(2, -1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0)
          values shouldBe Array("c", "b")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        removeValue(4, 5, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 0, 2)
          values shouldBe Array("f", "e", "d", "c", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        removeValue(3, 4, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 0, 2)
          values shouldBe Array("f", "e", "d", "b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        removeValue(2, 5, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 1, 3)
          values shouldBe Array("f", "e", "c", "b", "a")
          delta shouldBe -1
      }
    }

    "remove tree at index" in {
      testWithBuffers[String, Int](removeTree(0, -1, _, _), IntBuffer.empty, Buffer.empty[String]) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](removeTree(0, -1, _, _), IntBuffer(0), Buffer("a")) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeTree(0, 1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeTree(1, -1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe -2
      }
      testWithBuffers[String, Int](removeTree(1, 2, _, _), IntBuffer(0, 1, 1), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](removeTree(2, -1, _, _), IntBuffer(0, 1, 1), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe -3
      }
      testWithBuffers[String, Int](removeTree(1, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("c", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](removeTree(0, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        removeTree(4, 5, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("f", "e", "d", "a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](
        removeTree(2, 5, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe -3
      }
      testWithBuffers[String, Int](
        removeTree(1, 2, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 0, 1, 2)
          values shouldBe Array("f", "d", "c", "b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        removeTree(5, -1, _, _),
        IntBuffer(0, 0, 2, 0, 1, 2),
        Buffer("f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe -6
      }
    }

    "merge two trees - do nothing if indexes outside a range or equal" in {
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, 3, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, -1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(3, 1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe 3
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(-1, 1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe -1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, 1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(0, 0, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
          index shouldBe 0
      }
    }

    "merge two trees - smallest use-case, double node tree" in {
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, 0, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe -1
          index shouldBe 0
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(0, 1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0)
          values shouldBe Array("b")
          delta shouldBe -1
          index shouldBe 0
      }
    }

    "merge two trees - three node tree" in {
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("c", "b")
          delta shouldBe -1
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(0, 2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "c")
          delta shouldBe -1
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(1, 0, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
          index shouldBe 0
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(2, 0, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
          index shouldBe 1
      }
      testWithBuffers[String, (Int, Int)](mergeTwoTrees(2, 1, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("c", "a")
          delta shouldBe -1
          index shouldBe 1
      }
    }

    "merge two trees - larger trees" in {
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(3, 6, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 0, 0, 1, 3, 1)
          values shouldBe Array("d", "c", "h", "g", "f", "e", "a")
          delta shouldBe -1
          index shouldBe 5
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(6, 3, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 0, 1, 3, 1)
          values shouldBe Array("h", "g", "f", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 5
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(6, 2, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 0, 0, 1, 2, 2)
          values shouldBe Array("h", "e", "g", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 5
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(2, 6, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 0, 2, 2, 1)
          values shouldBe Array("h", "d", "c", "g", "f", "e", "a")
          delta shouldBe -1
          index shouldBe 4
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(1, 6, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 1, 1, 2, 1)
          values shouldBe Array("h", "d", "c", "g", "f", "e", "a")
          delta shouldBe -1
          index shouldBe 3
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(6, 1, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 2, 0, 1, 1, 2)
          values shouldBe Array("h", "f", "e", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 5
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(6, 0, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 1, 0, 1, 1, 2)
          values shouldBe Array("g", "f", "e", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 5
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(0, 6, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 1, 0, 1, 2, 1)
          values shouldBe Array("d", "c", "h", "g", "f", "e", "a")
          delta shouldBe -1
          index shouldBe 2
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(3, 7, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 1, 0, 0, 1, 3)
          values shouldBe Array("d", "c", "b", "h", "g", "f", "e")
          delta shouldBe -1
          index shouldBe 6
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(7, 3, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 0, 1, 1, 3)
          values shouldBe Array("h", "g", "f", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 6
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(4, 2, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 1, 0, 1, 1, 1, 2)
          values shouldBe Array("h", "e", "g", "d", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 3
      }
      testWithBuffers[String, (Int, Int)](
        mergeTwoTrees(2, 4, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 2, 0, 1, 2)
          values shouldBe Array("h", "g", "f", "e", "c", "b", "a")
          delta shouldBe -1
          index shouldBe 2
      }
    }

    "find pair of values with duplicated key - left to right" in {
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "c")), _._2) shouldBe None
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "a")), _._2) shouldBe Some(
        ((1, "a"), (3, "a"))
      )
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "c"), (4, "b"), (5, "a")), _._2) shouldBe Some(
        ((2, "b"), (4, "b"))
      )
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "c"), (4, "b"), (5, "b")), _._2) shouldBe Some(
        ((2, "b"), (4, "b"))
      )
      findFirstDuplicatePair[(Int, String), String](Slice((1, "b"), (2, "b"), (3, "b"), (4, "a"), (5, "a")), _._2) shouldBe Some(
        ((1, "b"), (2, "b"))
      )
    }

    "find pair of values with duplicated key - right to left" in {
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "c")), _._2, rightToLeft = true) shouldBe None
      findFirstDuplicatePair[(Int, String), String](Slice((1, "a"), (2, "b"), (3, "a")), _._2, rightToLeft = true) shouldBe Some(
        ((3, "a"), (1, "a"))
      )
      findFirstDuplicatePair[(Int, String), String](
        Slice((1, "a"), (2, "b"), (3, "c"), (4, "b"), (5, "a")),
        _._2,
        rightToLeft = true
      ) shouldBe Some(((4, "b"), (2, "b")))
      findFirstDuplicatePair[(Int, String), String](
        Slice((1, "a"), (2, "b"), (3, "c"), (4, "b"), (5, "b")),
        _._2,
        rightToLeft = true
      ) shouldBe Some(((5, "b"), (4, "b")))
      findFirstDuplicatePair[(Int, String), String](
        Slice((1, "b"), (2, "b"), (3, "b"), (4, "a"), (5, "a")),
        _._2,
        rightToLeft = true
      ) shouldBe Some(((5, "a"), (4, "a")))
    }

    "merge children of the node - distinct nodes test cases" in {
      testWithBuffers[String, Int](makeChildrenDistinct(0, _, _), IntBuffer(0), Buffer("a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](makeChildrenDistinct(1, _, _), IntBuffer(0, 1), Buffer("b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](makeChildrenDistinct(2, _, _), IntBuffer(0, 1, 1), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](makeChildrenDistinct(2, _, _), IntBuffer(0, 0, 2), Buffer("c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 1, 0, 0, 3),
        Buffer("e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 0, 0, 3)
          values shouldBe Array("e", "d", "c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(6, _, _),
        IntBuffer(0, 1, 0, 2, 0, 0, 3),
        Buffer("g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 0, 2, 0, 0, 3)
          values shouldBe Array("g", "f", "e", "d", "c", "b", "a")
          delta shouldBe 0
      }
    }

    "merge children of the node - duplicated nodes test cases" in {
      testWithBuffers[String, Int](makeChildrenDistinct(2, _, _), IntBuffer(0, 0, 2), Buffer("b", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](makeChildrenDistinct(3, _, _), IntBuffer(0, 0, 0, 3), Buffer("b", "c", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](makeChildrenDistinct(3, _, _), IntBuffer(0, 0, 0, 3), Buffer("b", "b", "b", "a")) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 0, 0, 0, 4),
        Buffer("c", "b", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 0, 0, 0, 4),
        Buffer("c", "b", "b", "c", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("b", "c", "a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 1, 0, 1, 2),
        Buffer("d", "b", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("d", "c", "b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 1, 0, 1, 2),
        Buffer("c", "b", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe -2
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(6, _, _),
        IntBuffer(0, 1, 0, 1, 2, 0, 2),
        Buffer("f", "d", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 0, 1, 2, 0, 2)
          values shouldBe Array("f", "d", "e", "d", "c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(4, _, _),
        IntBuffer(0, 1, 0, 1, 2, 0, 2),
        Buffer("f", "d", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1, 0, 2)
          values shouldBe Array("f", "e", "d", "c", "b", "a")
          delta shouldBe -1
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(6, _, _),
        IntBuffer(0, 1, 0, 1, 2, 0, 2),
        Buffer("d", "c", "d", "c", "b", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1, 1)
          values shouldBe Array("d", "c", "b", "a")
          delta shouldBe -3
      }
      testWithBuffers[String, Int](
        makeChildrenDistinct(9, _, _),
        IntBuffer(0, 0, 2, 0, 1, 0, 1, 3, 0, 2),
        Buffer("e", "d", "c", "d", "c", "d", "c", "b", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1, 1)
          values shouldBe Array("e", "d", "c", "b", "a")
          delta shouldBe -5
      }
    }

    "insert trees distinct left side" in {
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector.empty, _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((-1, IntSlice(), Slice.empty[String])), _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("a"))), _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((0, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0)
          values shouldBe Array("b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("a"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((0, IntSlice(0), Slice("c"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((1, IntSlice(0), Slice("c"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("b", "c", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((1, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((2, IntSlice(0, 1), Slice("c", "b"))), _, _, 0),
        IntBuffer(0, 1, 1),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((2, IntSlice(0, 1), Slice("d", "b"))), _, _, 0),
        IntBuffer(0, 1, 1),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("c", "d", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((3, IntSlice(0, 0, 2), Slice("e", "c", "b"))), _, _, 0),
        IntBuffer(0, 0, 2, 1),
        Buffer("d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3, 1)
          values shouldBe Array("d", "c", "e", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((3, IntSlice(0, 0, 2), Slice("c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 2, 1),
        Buffer("d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("d", "c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((4, IntSlice(0, 1, 0, 2), Slice("e", "c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 1, 2, 1),
        Buffer("d", "f", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 2, 2, 1)
          values shouldBe Array("d", "f", "e", "c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertLeftSubtreeListDistinct(Vector((2, IntSlice(0, 1, 0, 2), Slice("e", "c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 1, 0, 2, 2)
          values shouldBe Array("c", "e", "c", "d", "b", "a")
          delta shouldBe 3
      }
    }

    "insert trees distinct right side" in {
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector.empty, _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((-1, IntSlice(), Slice.empty[String])), _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array.empty[Int]
          values shouldBe Array.empty[String]
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("a"))), _, _, 0),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((0, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0)
          values shouldBe Array("b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((-1, IntSlice(0), Slice("a"))), _, _, 0),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((0, IntSlice(0), Slice("c"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((1, IntSlice(0), Slice("c"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((1, IntSlice(0), Slice("b"))), _, _, 0),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((2, IntSlice(0, 1), Slice("c", "b"))), _, _, 0),
        IntBuffer(0, 1, 1),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((2, IntSlice(0, 1), Slice("d", "b"))), _, _, 0),
        IntBuffer(0, 1, 1),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("d", "c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((3, IntSlice(0, 0, 2), Slice("e", "c", "b"))), _, _, 0),
        IntBuffer(0, 0, 2, 1),
        Buffer("d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3, 1)
          values shouldBe Array("e", "d", "c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((3, IntSlice(0, 0, 2), Slice("c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 2, 1),
        Buffer("d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("d", "c", "b", "a")
          delta shouldBe 0
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((4, IntSlice(0, 1, 0, 2), Slice("e", "c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 1, 2, 1),
        Buffer("d", "f", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 2, 2, 1)
          values shouldBe Array("d", "e", "f", "c", "b", "a")
          delta shouldBe 1
      }
      testWithBuffers[String, Int](
        insertRightSubtreeListDistinct(Vector((2, IntSlice(0, 1, 0, 2), Slice("e", "c", "d", "b"))), _, _, 0),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 1, 0, 2, 2)
          values shouldBe Array("c", "e", "c", "d", "b", "a")
          delta shouldBe 3
      }
    }
  }

}
