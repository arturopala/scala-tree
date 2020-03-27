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

package com.github.arturopala.tree.util

import java.util.NoSuchElementException

import com.github.arturopala.tree.Tree
import com.github.arturopala.tree.Tree.ArrayTree

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Collection of operations on the linear, array-based, representation of the tree. */
object ArrayTree {

  /** List indexes of the children values of the parent node, if any.
    * @note tree structure as returned by [[com.github.arturopala.tree.Tree.toArrays]] */
  final def childrenIndexes(parentIndex: Int, treeStructure: Int => Int): List[Int] = {
    val numberOfChildren = treeStructure(parentIndex)
    if (numberOfChildren == 0) Nil
    else {
      var result: List[Int] = List(parentIndex - 1)
      var n = numberOfChildren - 1
      var i = parentIndex - 1
      while (n > 0 && i >= 0) {
        var a = treeStructure(i)
        while (a > 0) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        i = i - 1
        result = i :: result
        n = n - 1
      }
      result.reverse
    }
  }

  /** Appends children indexes to the buffer, starting from the given position. */
  final def writeChildrenIndexes(
    parentIndex: Int,
    treeStructure: Int => Int,
    buffer: IntBuffer,
    position: Int
  ): Int = {
    val numberOfChildren = treeStructure(parentIndex)
    if (numberOfChildren == 0) 0
    else {
      var pos = position + numberOfChildren - 1
      buffer(pos) = parentIndex - 1
      var n = numberOfChildren - 1
      var i = parentIndex - 1
      while (n > 0 && i >= 0) {
        var a = treeStructure(i)
        while (a > 0) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        i = i - 1
        pos = pos - 1
        buffer(pos) = i
        n = n - 1
      }
      numberOfChildren - n
    }
  }

  /** Finds an index of the parent node of the given node.
    * @note If index > size - 1 then returns size -1.  */
  final def parentIndex(index: Int, size: Int, treeStructure: Int => Int): Int = {
    var v = 1
    var i = index
    var c = true
    while (c && i < size - 1) {
      i = i + 1
      v = v - treeStructure(i)
      c = v > 0
      v = v + 1
    }
    i
  }

  /** Calculates the size of the tree starting at the given index. */
  final def treeSize(index: Int, treeStructure: Int => Int): Int = {
    var i = index
    var a = treeStructure(i)

    while (a > 0 && i > 0) {
      i = i - 1
      a = a - 1 + treeStructure(i)
    }

    index - i + 1
  }

  /** Iterates over tree's node indexes, depth first. */
  final def nodeIndexIterator(startIndex: Int, treeStructure: Int => Int): Iterator[Int] = new Iterator[Int] {

    var i: Int = startIndex
    var hasNext: Boolean = i >= 0
    var a: Int = if (hasNext) treeStructure(i) else 0

    override def next(): Int =
      if (hasNext) {
        val index = i
        hasNext = a > 0 && i > 0
        if (hasNext) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        index
      } else throw new NoSuchElementException
  }

  /** Iterates over tree's branches as index lists, depth first. */
  final def branchesIndexListIterator(startIndex: Int, treeStructure: Int => Int): Iterator[IntSlice] =
    new Iterator[IntSlice] {

      var hasNext: Boolean = false
      var array: IntSlice = IntSlice.empty

      val counters = new IntBuffer()
      val indexes = new IntBuffer()

      indexes.push(startIndex)
      seekNext(false)

      override def next(): IntSlice =
        if (hasNext) {
          val result = array
          seekNext(true)
          result
        } else throw new NoSuchElementException

      @tailrec
      def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false }
        else {
          val i = indexes.peek
          if (i < 0) { hasNext = false }
          else {
            val c = treeStructure(i)
            if (c == 0) {
              array = BranchIterator.readBranch(counters, indexes).push(i).toSlice
              hasNext = true
              BranchIterator.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
              seekNext(false)
            }
          }
        }
    }

  private object BranchIterator {

    def readBranch(counters: IntBuffer, indexes: IntBuffer): IntBuffer = {
      val branchIndexes = new IntBuffer()
      var i = 0
      var ci = 0
      while (ci < counters.length) {
        branchIndexes.push(indexes(i))
        val c = counters(ci)
        i = i + c
        ci = ci + 1
      }
      branchIndexes
    }

    def sizeBranch(counters: IntBuffer): Int = {
      var l = 0
      var i = 0
      var ci = 0
      while (ci < counters.length) {
        l = l + 1
        val c = counters(ci)
        i = i + c
        ci = ci + 1
      }
      l
    }

    def retract(counters: IntBuffer, indexes: IntBuffer): Unit =
      if (counters.length >= 1) {
        var value = counters.pop - 1
        indexes.pop
        while (value == 0 && counters.length >= 1) {
          value = counters.pop - 1
          indexes.pop
        }
        if (value != 0) counters.push(value)
      }
  }

  /** Iterates over tree's branches with pred. */
  final def branchIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntSlice, Iterable[T]](
      branchesIndexListIterator(startIndex, treeStructure),
      _.map(treeValues).asIterable,
      pred
    )

  /** Iterates over all subtrees (including the tree itself), depth-first. */
  final def treeIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    new MapFilterIterator[Int, Tree[T]](
      nodeIndexIterator(startIndex, treeStructure),
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Returns tree rooted at the given index. */
  final def treeAt[T: ClassTag](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): Tree[T] = {
    val size = treeSize(index, treeStructure)
    if (size == 0) Tree.empty
    else {
      val structure = treeStructure.slice(index - size + 1, index + 1)
      val values = treeValues.slice(index - size + 1, index + 1)
      new ArrayTree[T](structure, values, structure.count(_ == 0), calculateHeight(structure.length - 1, structure))
    }
  }

  private val readBranchSlice: (IntBuffer, IntBuffer, Int) => IntSlice =
    (counters, indexes, i) => BranchIterator.readBranch(counters, indexes).push(i).toSlice

  /** Fold tree's branches as index lists. */
  final def foldLeftBranchesIndexLists[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, IntSlice, Int) => A
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchSlice, fold)

  private val readBranchLength: (IntBuffer, IntBuffer, Int) => Int =
    (counters, _, _) => BranchIterator.sizeBranch(counters) + 1

  /** Fold tree's branch lengths. */
  final def foldLeftBranchesLengths[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, Int, Int) => A
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchLength, fold)

  final def foldLeftBranches[A, R](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    read: (IntBuffer, IntBuffer, Int) => R,
    fold: (A, R, Int) => A
  ): A = {

    var value = initialValue

    if (startIndex >= 0) {

      val counters = new IntBuffer()
      val indexes = new IntBuffer()

      var n = 0

      indexes.push(startIndex)

      do {
        val i = indexes.peek
        val c = treeStructure(i)
        if (c == 0) {
          val newValue = read(counters, indexes, i)
          value = fold(value, newValue, n)
          BranchIterator.retract(counters, indexes)
          n = n + 1
        } else {
          counters.push(c)
          writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
        }
      } while (counters.nonEmpty)
    }

    value
  }

  /** Calculates the height of the tree, i.e. the length of the longest branch. */
  @`inline` final def calculateHeight(startIndex: Int, treeStructure: Int => Int): Int =
    foldLeftBranchesLengths(startIndex, treeStructure, 0, (a: Int, v: Int, _: Int) => Math.max(a, v))

  /** Count branches fulfilling the predicate. */
  final def countBranches[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: List[T] => Boolean
  ): Int =
    foldLeftBranchesIndexLists(
      startIndex,
      treeStructure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.reverseIterator.map(treeValues).toList)) 1 else 0)
    )

  /** Renders tree as a string iterating over the branches.*/
  final def mkStringUsingBranches[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int
  ): StringBuilder = {

    def renderBranch(builder: StringBuilder, branch: IntSlice, branchIndex: Int): StringBuilder = {
      if (branchIndex > 0) builder.append(branchSeparator)
      builder.append(branchStart)
      var i = 0
      val iter = branch.iterator
      while (iter.hasNext) {
        if (i > 0) builder.append(valueSeparator)
        builder.append(treeValues(iter.next()))
        i = i + 1
      }
      builder.append(branchEnd)
    }

    foldLeftBranchesIndexLists(startIndex, treeStructure, new StringBuilder(), renderBranch)
  }

  /** Follows the given path into the tree.
    * @return a tuple consisting of:
    *         - an array of travelled indexes,
    *         - optionally non matching path segment,
    *         - remaining path iterator,
    *         - flag set to true if path matched an entire branch.
    *
    * @note 1. Assumes distinct values of children in each node.
    *       2. To check if the path exists within the tree it is sufficient to validate
    *          that returned non matching segment is empty.
    *       3. Generally, if the tree contains the path then the returned tuple
    *          will be (non empty indexes, None, empty iterator, true or false),
    *          but if the tree does not contain the path then the returned tuple
    *          will be (indexes (maybe empty), Some(segment), some iterator (maybe empty), true or false).
    */
  final def followPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): (Array[Int], Option[T1], Iterator[T1], Boolean) = {

    val indexes = new IntBuffer() // travelled indexes
    val children = new IntBuffer().push(startIndex) // children indexes to consider
    val pathIterator = path.iterator

    var pathSegment: Option[T1] = None

    while (children.nonEmpty && pathIterator.hasNext) {
      val c = children.reset
      pathSegment = Some(pathIterator.next())
      var n = 0
      while (n >= 0 && n < c) {
        val ci = children(n) // child index
        if (ci >= 0 && pathSegment.contains(treeValues(ci))) {
          indexes.push(ci)
          writeChildrenIndexes(ci, treeStructure, children, 0)
          pathSegment = None
          n = -1 // force inner loop exit
        } else {
          n = n + 1
        }
      }
    }

    (indexes.toArray, pathSegment, pathIterator, pathSegment.isEmpty && children.isEmpty)
  }

  @`inline` final def containsBranch[T, T1 >: T](
    branch: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, fullMatch) = followPath(branch, startIndex, treeStructure, treeValues)
    fullMatch && unmatched.isEmpty
  }

  @`inline` final def containsPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, _) = followPath(path, startIndex, treeStructure, treeValues)
    unmatched.isEmpty
  }

  @`inline` final def selectValue[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[T] =
    followPath(path, startIndex, treeStructure, treeValues) match {
      case (indexes, None, _, _) if indexes.nonEmpty => Some(treeValues(indexes.last))
      case _                                         => None
    }

  @`inline` final def selectTree[T: ClassTag, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Option[Tree[T]] =
    followPath(path, startIndex, treeStructure, treeValues) match {
      case (indexes, None, _, _) if indexes.nonEmpty =>
        val tree = treeAt[T](indexes.last, treeStructure, treeValues)
        Some(tree)

      case _ => None
    }

}
