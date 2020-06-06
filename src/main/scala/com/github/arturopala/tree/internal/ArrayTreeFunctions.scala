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

package com.github.arturopala.tree.internal

import java.util.NoSuchElementException

import com.github.arturopala.bufferandslice.IndexTracker.{trackMoveRangeLeft, trackMoveRangeRight, trackShiftLeft}
import com.github.arturopala.bufferandslice.{Buffer, IndexTracker, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.internal.ArrayTreeFunctions.parentIndex

import scala.annotation.tailrec
import scala.collection.Iterator.continually
import scala.collection.mutable

/**
  * Essential low-level operations on the linear encoding of the tree.
  *
  * @note the functions presented here returns indexes, values, or mutates buffers in-place.
  *       For any high-level transformations look into [[ArrayTree]].
  *
  *       The rule of thumb is functions here shouldn't depend on [[ArrayTree]],
  *       and shouldn't know about the [[com.github.arturopala.tree.Tree]] type.
  */
object ArrayTreeFunctions {

  @`inline` private def identity[T, T1 >: T]: T => T1 = x => x

  /** Finds an index of the parent node of the given node.
    * @note If index > size - 1 then returns -1.  */
  @`inline` final def parentIndex(index: Int, treeStructure: IntBuffer): Int =
    parentIndex(index, treeStructure.length, treeStructure)

  /** Finds an index of the parent node of the given node.
    * @note If index > size - 1 then returns -1.  */
  @`inline` final def parentIndex(index: Int, treeStructure: IntSlice): Int =
    parentIndex(index, treeStructure.length, treeStructure)

  /** Finds an index of the parent node of the given node.
    * @note If index > size - 1 then returns -1.  */
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
    if (c) -1 else i
  }

  /** Calculates leftmost (bottom) index of the tree rooted at index. */
  final def bottomIndex(index: Int, treeStructure: Int => Int): Int =
    if (treeStructure(index) == 0) index
    else index - treeSize(index, treeStructure) + 1

  /** Calculates the size of the tree starting at index. */
  final def treeSize(index: Int, treeStructure: Int => Int): Int = {
    var i = index
    var a = treeStructure(i)

    while (a > 0 && i > 0) {
      i = i - 1
      a = a - 1 + treeStructure(i)
    }

    val size = index - i + 1

    if (a == 0) size
    else throw new IllegalArgumentException(s"Incomplete tree, size is $size, but missing at least $a node(s)")
  }

  /** Index of the first child of the parentIndex. */
  @`inline` final def firstChildIndex(parentIndex: Int, treeStructure: Int => Int): Option[Int] =
    if (parentIndex >= 0 && treeStructure(parentIndex) > 0) Some(parentIndex - 1)
    else None

  /** Index of the last child of the parentIndex. */
  final def lastChildIndex(parentIndex: Int, treeStructure: Int => Int): Option[Int] =
    if (parentIndex < 0 || treeStructure(parentIndex) < 1) None
    else {
      var n = treeStructure(parentIndex) - 1
      var i = parentIndex - 1
      while (n > 0 && i >= 0) {
        var a = treeStructure(i)
        while (a > 0) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        i = i - 1
        n = n - 1
      }
      Some(i)
    }

  /** Lists indexes of the children values of the parent node, if any. */
  final def childrenIndexes(parentIndex: Int, treeStructure: Int => Int): IntBuffer =
    if (parentIndex >= 0) {
      val numberOfChildren = treeStructure(parentIndex)
      val result = new IntBuffer(numberOfChildren)
      if (numberOfChildren > 0) {
        result.push(parentIndex - 1)
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          result.push(i)
          n = n - 1
        }
      }
      result
    } else IntBuffer.empty

  /** Iterates over indexes of the children of the parent node, if any. */
  final def childrenIndexesIterator(parentIndex: Int, treeStructure: Int => Int): Iterator[Int] = new Iterator[Int] {

    var n = treeStructure(parentIndex)
    var i = parentIndex - 1

    final override def hasNext: Boolean = n > 0

    final override def next: Int =
      if (n <= 0) throw new NoSuchElementException()
      else {
        val item = i
        var a = treeStructure(i)
        while (a > 0) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        i = i - 1
        n = n - 1
        item
      }
  }

  /** Lists indexes, in the reverse order, of the children values of the parent node, if any. */
  final def childrenIndexesReverse(parentIndex: Int, treeStructure: Int => Int): IntBuffer =
    if (parentIndex >= 0) {
      val numberOfChildren = treeStructure(parentIndex)
      val result = new IntBuffer(numberOfChildren)
      if (numberOfChildren > 0) {
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        result(n) = i
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          n = n - 1
          result(n) = i
        }
      }
      result
    } else IntBuffer.empty

  /** Returns a list of children of a tree represented by the structure and content slices. */
  @`inline` final def childrenOf[T](treeStructure: IntSlice, treeValues: Slice[T]): Iterator[(IntSlice, Slice[T])] =
    childrenIndexesIterator(treeStructure.top, treeStructure)
      .map(treeAt(_, treeStructure, treeValues))

  /** Returns a reversed list of children of a tree represented by the structure and content slices. */
  @`inline` final def reversedChildrenOf[T](
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Iterator[(IntSlice, Slice[T])] =
    childrenIndexes(treeStructure.top, treeStructure).reverseIterator
      .map(treeAt(_, treeStructure, treeValues))

  /** Returns a structure and content slices representing a subtree rooted at the given index. */
  final def treeAt[T](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): (IntSlice, Slice[T]) = {
    val size = treeSize(index, treeStructure)
    val structure = treeStructure.slice(index - size + 1, index + 1)
    val values = treeValues.slice(index - size + 1, index + 1)
    (structure, values)
  }

  /** Returns a structure and content slices representing a subtree rooted at the given index. */
  final def treeAt[T](index: Int, treeStructure: IntBuffer, treeValues: Buffer[T]): (IntSlice, Slice[T]) = {
    val size = treeSize(index, treeStructure)
    val structure = treeStructure.slice(index - size + 1, index + 1)
    val values = treeValues.slice(index - size + 1, index + 1)
    (structure, values)
  }

  /** Writes children indexes to the buffer range of [position, position + numberOfChildren).
    * @return number of children appended
    */
  final def writeChildrenIndexesToBuffer(
    parentIndex: Int,
    treeStructure: Int => Int,
    buffer: IntBuffer,
    position: Int
  ): Int =
    if (parentIndex < 0) 0
    else {
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

  /** Finds an index of a sibling of childIndex having some value.
    * Calculates parentIndex.
    * @return some nearest index if exists or none*/
  final def nearestSiblingHavingValue[T](
    value: T,
    childIndex: Int,
    size: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    val parent = parentIndex(childIndex, size, treeStructure)
    findNearestSiblingHavingValue(parent, value, childIndex, treeStructure, treeValues)
  }

  /** Finds an index of a sibling of childIndex having some value.
    * @return some nearest index if exists or none*/
  final def findNearestSiblingHavingValue[T](
    parentIndex: Int,
    value: T,
    childIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] =
    if (parentIndex < 0) None
    else {
      val children = childrenHavingValue(value, parentIndex, treeStructure, treeValues)
      if (children.isEmpty) None
      else {
        var result: Option[Int] = None
        var i = 0
        while (i >= 0 && i < children.length) {
          if (children(i) == childIndex) {
            result =
              if (i > 0) Some(children(i - 1))
              else if (i < children.top) Some(children(i + 1))
              else None
            i = -1
          } else if (children(i) < childIndex) {
            result = Some(if (i > 0) children(i - 1) else children(i))
            i = -1
          } else {
            i = i + 1
          }
        }
        if (i == children.length && children.nonEmpty) Some(children.last)
        else result
      }
    }

  /** Looks for Some index of the child node holding the given value, or None.
    * @return some first index if exists or none */
  final def firstChildHavingValue[T](
    value: T,
    parentIndex: Int,
    size: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    var result: Option[Int] = None
    if (parentIndex >= 0 && parentIndex < size) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = Some(parentIndex - 1)
        } else {
          var n = numberOfChildren - 1
          var i = parentIndex - 1
          while (n > 0 && i >= 0) {
            var a = treeStructure(i)
            while (a > 0) {
              i = i - 1
              a = a - 1 + treeStructure(i)
            }
            i = i - 1
            if (treeValues(i) == value) {
              result = Some(i)
              n = -1
            }
            n = n - 1
          }
        }
      }
    } else if (size > 0 && treeValues(size - 1) == value) {
      result = Some(size - 1)
    }
    result
  }

  /** Looks for Some index of the child node holding the given value, or None.
    * @return some last index if exists or none */
  final def lastChildHavingValue[T](
    value: T,
    parentIndex: Int,
    size: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    var result: Option[Int] = None
    if (parentIndex >= 0 && parentIndex < size) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = Some(parentIndex - 1)
        }
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          if (treeValues(i) == value) {
            result = Some(i)
          }
          n = n - 1
        }

      }
    } else if (size > 0 && treeValues(size - 1) == value) {
      result = Some(size - 1)
    }
    result
  }

  /** List indexes of the children values of the parent node holding the given value, if any.
    * @return a slice of array of indexes ordered right to left (descending)
    */
  final def childrenHavingValue[T](
    value: T,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): IntSlice = {
    var result = new IntBuffer(8)
    if (parentIndex > 0) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = result.push(parentIndex - 1)
        }
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          if (treeValues(i) == value) {
            result = result.push(i)
          }
          n = n - 1
        }
      }
    }
    result.asSlice
  }

  /** Iterates over tree's node indexes, top-down, depth first. */
  final def nodesIndexIteratorDepthFirst(startIndex: Int, treeStructure: Int => Int): Iterator[Int] =
    new Iterator[Int] {

      private var i: Int = startIndex
      var hasNext: Boolean = i >= 0
      private var a: Int = if (hasNext) treeStructure(i) else 0

      final override def next(): Int =
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

  /** Iterates over tree's node indexes, top-down, breadth-first. */
  final def nodesIndexIteratorBreadthFirst(startIndex: Int, treeStructure: Int => Int): Iterator[Int] =
    new Iterator[Int] {

      private val queue: IntBuffer =
        if (startIndex < 0) IntBuffer.empty
        else IntBuffer(startIndex)

      final def hasNext: Boolean = queue.nonEmpty

      final override def next(): Int =
        if (hasNext) {
          val index = queue.pop
          val numberOfChildren = treeStructure(index)
          if (numberOfChildren > 0) {
            queue.shiftRight(0, numberOfChildren)
            writeChildrenIndexesToBuffer(index, treeStructure, queue, 0)
          }
          index
        } else throw new NoSuchElementException
    }

  /** Iterates over tree's node indexes down to the specified depth, top-down, depth first. */
  final def nodesIndexIteratorDepthFirstWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[Int] =
    new Iterator[Int] {

      var hasNext: Boolean = false
      private var i: Int = startIndex

      private val counters = new IntBuffer(8)
      private val indexes = new IntBuffer(8)

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      final override def next(): Int =
        if (hasNext) {
          val result = i
          seekNext(true)
          result
        } else throw new NoSuchElementException

      final def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false }
        else {
          i = indexes.peek
          if (i < 0) { hasNext = false }
          else {
            hasNext = true
            val c = treeStructure(i)
            if (c == 0 || counters.length >= maxDepth - 1) {
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexesToBuffer(i, treeStructure, indexes, indexes.length)
            }
          }
        }
    }

  /** Iterates over pairs of (level, index) down to the specified depth, top-down, depth first.
    * @return iterator over tuples of (level, index) */
  final def nodesIndexAndLevelIteratorDepthFirstWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[(Int, Int)] =
    new Iterator[(Int, Int)] {

      var hasNext: Boolean = false
      private var i: (Int, Int) = (1, startIndex)

      private val counters = new IntBuffer(8)
      private val indexes = new IntBuffer(8)

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      final override def next(): (Int, Int) =
        if (hasNext) {
          val result = i
          seekNext(true)
          result
        } else throw new NoSuchElementException

      final def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false }
        else {
          val index = indexes.peek
          i = (counters.length + 1, index)
          if (index < 0) { hasNext = false }
          else {
            hasNext = true
            val c = treeStructure(index)
            if (c == 0 || counters.length >= maxDepth - 1) {
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexesToBuffer(index, treeStructure, indexes, indexes.length)
            }
          }
        }
    }

  /** Iterates over tree's node indexes down to the specified depth, top-down, breadth-first. */
  final def nodesIndexIteratorBreadthFirstWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[Int] =
    new Iterator[Int] {

      private val queue: IntBuffer =
        if (startIndex < 0 || maxDepth < 1) IntBuffer.empty
        else IntBuffer(startIndex)

      private val levels: IntBuffer =
        if (startIndex < 0 || maxDepth < 1) IntBuffer.empty
        else IntBuffer(1)

      final def hasNext: Boolean = queue.nonEmpty

      final override def next(): Int =
        if (hasNext) {
          val index = queue.pop
          val level = levels.pop
          if (level < maxDepth) {
            val numberOfChildren = treeStructure(index)
            if (numberOfChildren > 0) {
              queue.shiftRight(0, numberOfChildren)
              writeChildrenIndexesToBuffer(index, treeStructure, queue, 0)
              levels.insertFromIterator(0, numberOfChildren, continually(level + 1))
            }
          }
          index
        } else throw new NoSuchElementException
    }

  /** Iterates over pairs of (level, index) down to the specified depth, top-down, breadth-first.
    * @return iterator over tuples of (level, index) */
  final def nodesIndexAndLevelIteratorBreadthFirstWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[(Int, Int)] =
    new Iterator[(Int, Int)] {

      private val queue: IntBuffer =
        if (startIndex < 0 || maxDepth < 1) IntBuffer.empty
        else IntBuffer(startIndex)

      private val levels: IntBuffer =
        if (startIndex < 0 || maxDepth < 1) IntBuffer.empty
        else IntBuffer(1)

      final def hasNext: Boolean = queue.nonEmpty

      final override def next(): (Int, Int) =
        if (hasNext) {
          val index = queue.pop
          val level = levels.pop
          if (level < maxDepth) {
            val numberOfChildren = treeStructure(index)
            if (numberOfChildren > 0) {
              queue.shiftRight(0, numberOfChildren)
              writeChildrenIndexesToBuffer(index, treeStructure, queue, 0)
              levels.insertFromIterator(0, numberOfChildren, continually(level + 1))
            }
          }
          (level, index)
        } else throw new NoSuchElementException
    }

  /** Iterates over tree's branches as index lists, depth first. */
  final def branchesIndexListIterator(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[IntBuffer] =
    new Iterator[IntBuffer] {

      var hasNext: Boolean = false
      private var array: IntBuffer = IntBuffer.empty

      private val counters = new IntBuffer(8)
      private val indexes = new IntBuffer(8)

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      final override def next(): IntBuffer =
        if (hasNext) {
          val result = array
          seekNext(true)
          result
        } else throw new NoSuchElementException

      @tailrec
      final def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false }
        else {
          val i = indexes.peek
          if (i < 0) { hasNext = false }
          else {
            val c = treeStructure(i)
            if (c == 0 || counters.length >= maxDepth - 1) {
              array = BranchIteratorUtils.readBranch(counters, indexes).push(i)
              hasNext = true
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexesToBuffer(i, treeStructure, indexes, indexes.length)
              seekNext(false)
            }
          }
        }
    }

  private object BranchIteratorUtils {

    final def readBranch(counters: IntBuffer, indexes: IntBuffer): IntBuffer = {
      val branchIndexes = new IntBuffer(8)
      var i = 0
      var ci = 0
      while (ci < counters.length) { // for all counters
        branchIndexes.push(indexes(i)) // read and push index at position
        val c = counters(ci) // read next child ordinal
        i = i + c // advance position to the next child
        ci = ci + 1 // increment counter ordinal
      }
      branchIndexes
    }

    final def sizeBranch(counters: IntBuffer): Int = {
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

    final def retract(counters: IntBuffer, indexes: IntBuffer): Unit =
      if (counters.length >= 1) {
        var value = counters.pop - 1
        indexes.pop
        while (value == 0 && counters.length >= 1) {
          value = counters.pop - 1
          indexes.pop
        }
        if (value != 0) counters.push(value)
      }

    final def retract(counters: IntBuffer): Unit =
      if (counters.length >= 1) {
        var value = counters.pop - 1
        while (value == 0 && counters.length >= 1) {
          value = counters.pop - 1
        }
        if (value != 0) counters.push(value)
      }
  }

  @`inline` final val readBranchSlice: (IntBuffer, IntBuffer, Int) => IntSlice =
    (counters, indexes, i) => BranchIteratorUtils.readBranch(counters, indexes).push(i).asSlice

  /** Fold tree's branches as index lists. */
  final def foldLeftBranchesIndexLists[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, IntSlice, Int) => A,
    maxDepth: Int = Int.MaxValue
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchSlice, fold, maxDepth)

  @`inline` final val readBranchLength: (IntBuffer, IntBuffer, Int) => Int =
    (counters, _, _) => BranchIteratorUtils.sizeBranch(counters) + 1

  /** Fold tree's branch lengths. */
  final def foldLeftBranchesLengths[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, Int, Int) => A
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchLength, fold)

  /** Fold tree branches using provided read and fold functions.
    * @param read function to read counters and indexes buffer into result
    * @param fold function to fold value, result and branch number into accumulated value
    * @param maxDepth maximum depth of the tree to travel
    * */
  final def foldLeftBranches[A, R](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    read: (IntBuffer, IntBuffer, Int) => R,
    fold: (A, R, Int) => A,
    maxDepth: Int = Int.MaxValue
  ): A = {

    var result = initialValue

    if (startIndex >= 0 && maxDepth > 0) {

      val counters = new IntBuffer(8)
      val indexes = new IntBuffer(8)

      var n = 0

      indexes.push(startIndex)

      while ({
        val i = indexes.peek
        if (i < 0) {
          val value = read(counters, indexes, 0)
          result = fold(result, value, n)
          BranchIteratorUtils.retract(counters, indexes)
          n = n + 1
        } else {
          val c = treeStructure(i)
          if (c == 0 || counters.length >= maxDepth - 1) {
            val newValue = read(counters, indexes, i)
            result = fold(result, newValue, n)
            BranchIteratorUtils.retract(counters, indexes)
            n = n + 1
          } else {
            counters.push(c)
            writeChildrenIndexesToBuffer(i, treeStructure, indexes, indexes.length)
          }
        }
        counters.nonEmpty
      }) ()
    }

    result
  }

  /** Calculates the height of the tree, i.e. the length of the longest branch. */
  @`inline` final def calculateWidth(treeStructure: IntSlice): Int =
    treeStructure.count(_ == 0)

  /** Calculates the height of the tree, i.e. the length of the longest branch. */
  @`inline` final def calculateHeight(treeStructure: IntSlice): Int =
    calculateHeight(treeStructure.top, treeStructure)

  /** Calculates the height of the tree at index, i.e. the length of the longest branch. */
  @`inline` final def calculateHeight(startIndex: Int, treeStructure: Int => Int): Int =
    foldLeftBranchesLengths(startIndex, treeStructure, 0, (a: Int, v: Int, _: Int) => Math.max(a, v))

  /** Renders tree as a string iterating over the branches.*/
  final def mkStringFromBranches[T](
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
        builder.append(show(treeValues(iter.next())))
        i = i + 1
      }
      builder.append(branchEnd)
    }

    foldLeftBranchesIndexLists(startIndex, treeStructure, new StringBuilder(), renderBranch, maxDepth)
  }

  /** Follows the entire path of values into the tree.
    * @return a Some of an array of travelled indexes, or None if path doesn't exist.
    */
  @`inline` final def followEntirePath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[IntSlice] =
    followPath(path, startIndex, treeStructure, treeValues) match {
      case (indexes, None, _, _) if indexes.nonEmpty => Some(indexes)
      case _                                         => None
    }

  /** Follows the given path of values into the tree.
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
  @`inline` final def followPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): (IntSlice, Option[T1], Iterator[T1], Boolean) =
    followPath(path, startIndex, treeStructure, treeValues, identity[T, T1])

  /** Follows the entire path of into the tree using a path item extractor function.
    * @return a Some of an array of travelled indexes, or None if path doesn't exist.
    */
  @`inline` final def followEntirePath[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Option[IntSlice] =
    followPath(path, startIndex, treeStructure, treeValues, toPathItem) match {
      case (indexes, None, _, _) if indexes.nonEmpty => Some(indexes)
      case _                                         => None
    }

  /** Follows the given path into the tree using a path item extractor function.
    * @param toPathItem function to extract path item from the tree's node value.
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
  final def followPath[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): (IntSlice, Option[K], Iterator[K], Boolean) = {

    val indexes = new IntBuffer(8) // travelled indexes
    val children = new IntBuffer(8).push(startIndex) // children indexes to consider
    val pathIterator = path.iterator

    var pathSegment: Option[K] = None

    while (children.nonEmpty && pathIterator.hasNext) {
      pathSegment = Some(pathIterator.next())
      if (children.nonEmpty) {
        val c = children.reset + 1
        var n = 0
        while (n >= 0 && n < c) {
          val ci = children(n) // child index
          if (ci >= 0 && pathSegment.contains(toPathItem(treeValues(ci)))) {
            indexes.push(ci)
            writeChildrenIndexesToBuffer(ci, treeStructure, children, 0)
            pathSegment = None
            n = -1 // force inner loop exit
          } else {
            n = n + 1
          }
        }
      }
    }

    if (pathSegment.isEmpty && pathIterator.hasNext) {
      pathSegment = Some(pathIterator.next())
    }

    (indexes.asSlice, pathSegment, pathIterator, pathSegment.isEmpty && children.isEmpty)
  }

  /** Inserts new child value of the parent at an index..
    * Shifts existing buffer content right, starting from an index.
    * Increments parent's children count.
    * @return buffer length change */
  def insertValue[T](
    index: Int,
    parentIndex: Int,
    value: T,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int = {
    if (parentIndex >= 0) {
      structureBuffer.increment(parentIndex)
    }
    structureBuffer.shiftRight(index, 1)
    structureBuffer.update(index, 0)
    valuesBuffer.shiftRight(index, 1)
    valuesBuffer.update(index, value)
    1
  }

  /** Inserts slice's content to the buffers at an index.
    * Shifts existing buffer content right, starting from an index.
    * @return buffer length change */
  final def insertSlice[T](
    index: Int,
    structure: IntSlice,
    values: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int = {
    structureBuffer.insertSlice(Math.max(index, 0), structure)
    valuesBuffer.insertSlice(Math.max(index, 0), values)
    structure.length
  }

  /** Inserts content provided by iterators to the buffers at an index.
    * Shifts existing buffer content right, starting from an index, at length.
    * @return buffer length change */
  final def insertFromIterator[T](
    index: Int,
    length: Int,
    structure: Iterator[Int],
    values: Iterator[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int = {
    structureBuffer.insertFromIterator(Math.max(index, 0), length, structure)
    valuesBuffer.insertFromIterator(Math.max(index, 0), length, values)
    length
  }

  /** Inserts in the reverse order content provided by iterators to the buffers at an index.
    * Shifts existing buffer content right, starting from an index, at length.
    * @return buffer length change */
  final def insertFromIteratorReverse[T](
    index: Int,
    length: Int,
    structure: Iterator[Int],
    values: Iterator[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int = {
    structureBuffer.insertFromIteratorReverse(Math.max(index, 0), length, structure)
    valuesBuffer.insertFromIteratorReverse(Math.max(index, 0), length, values)
    length
  }

  /** Removes value at index and joins its subtrees to the parent node,
    * @param keepDistinct if true, checks newly added orphans are distinct.
    */
  final def removeValue[T](
    index: Int,
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int = {
    val orphans = ArrayTreeFunctions.childrenIndexes(index, structureBuffer)
    val delta1 = ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, valuesBuffer)
    val delta2 = if (keepDistinct && orphans.nonEmpty) {
      var d = 0
      while (orphans.nonEmpty) {
        val i = orphans.head
        d = d + ArrayTreeFunctions.ensureChildDistinct(i, structureBuffer, valuesBuffer, orphans.tail)
      }
      d
    } else 0
    delta1 + delta2

  }

  /** Removes value at index and joins subtrees to the parent node.
    * Does not check for duplicate children values in parent. */
  final def removeValue[T](index: Int, parentIndex: Int, structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Int =
    if (index >= 0 && index < structureBuffer.length) {
      if (parentIndex >= 0) {
        structureBuffer.modify(parentIndex, _ + structureBuffer(index) - 1)
      }
      structureBuffer.shiftLeft(index + 1, 1)
      valuesBuffer.shiftLeft(index + 1, 1)
      -1
    } else 0

  /** Removes a whole tree at the index. */
  final def removeTree[T](index: Int, parentIndex: Int, structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Int =
    if (index >= 0 && index < structureBuffer.length) {
      val size = treeSize(index, structureBuffer)
      if (parentIndex >= 0) {
        structureBuffer.decrement(parentIndex)
      }
      structureBuffer.removeRange(index - size + 1, index + 1)
      valuesBuffer.removeRange(index - size + 1, index + 1)
      -size
    } else 0

  /** Removes all the children of the tree at the index. */
  final def removeChildren[T](index: Int, parentIndex: Int, structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Int =
    if (index >= 0 && index < structureBuffer.length) {
      val size = treeSize(index, structureBuffer)
      structureBuffer(index) = 0
      structureBuffer.removeRange(index - size + 1, index)
      valuesBuffer.removeRange(index - size + 1, index)
      -size + 1
    } else 0

  /** Inserts branch into the buffers with checking for duplicates.
    * @param branchIterator list of values
    * @return buffer length change */
  @tailrec
  final def insertBranch[T](
    branchIterator: Iterator[T],
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    if (branchIterator.hasNext) {
      val value = branchIterator.next()
      lastChildHavingValue(value, parentIndex, structureBuffer.length, structureBuffer, valuesBuffer) match {
        case Some(index) =>
          insertBranch(branchIterator, index, structureBuffer, valuesBuffer, offset)

        case None =>
          val insertIndex = if (parentIndex >= 0) {
            structureBuffer.increment(parentIndex)
            parentIndex
          } else 0
          val l0 = valuesBuffer.length
          valuesBuffer.insert(insertIndex, value)
          valuesBuffer.insertFromIteratorReverse(insertIndex, branchIterator)
          val delta = valuesBuffer.length - l0
          structureBuffer.insert(insertIndex, 0)
          structureBuffer.insertFromIterator(insertIndex + 1, delta - 1, continually(1))
          delta
      }
    } else offset

  /** Inserts tree to the buffers at the given index, and keeps children distinct.
    * Does nothing when an empty tree. Uses unsafe recursion.
    * @return buffers length change */
  final def insertChildDistinctUnsafe[T](
    childStructure: IntSlice,
    childValues: Slice[T],
    index: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (childStructure.length == 0) 0
    else {
      valuesBuffer(index) = childValues.last
      childrenOf(childStructure, childValues).foldLeft(0) {
        case (offset, (structure, values)) =>
          val bufferIndex = index + offset
          offset + (firstChildHavingValue(
            childValues.last,
            bufferIndex,
            structureBuffer.length,
            structureBuffer,
            valuesBuffer
          ) match {
            case None =>
              val s = treeSize(bufferIndex, structureBuffer) - 1
              structureBuffer.increment(bufferIndex)
              insertSlice(bufferIndex - s, childStructure, childValues, structureBuffer, valuesBuffer)

            case Some(i) => insertChildDistinctUnsafe(structure, values, i, structureBuffer, valuesBuffer)
          })
      }
    }

  /** Inserts child to the buffers at the given index, and keeps children distinct. */
  @`inline` final def insertBetweenChildrenDistinct[T](
    childIndex: Int,
    childStructure: IntSlice,
    childValues: Slice[T],
    insertAfter: Boolean,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer = IntBuffer.empty
  ): Int =
    insertBetweenChildrenDistinct(
      Vector((childIndex, childStructure, childValues, insertAfter)),
      structureBuffer,
      valuesBuffer,
      0,
      indexesToTrack
    )

  /** Inserts children into the buffers with checking for duplicates.
    * Each child is inserted on the specified index or merged down with the nearest duplicate.
    * Does nothing for an empty tree.
    * @param queue sequence of tuples of (proposed index, tree structure, tree values, place after flag)
    * @return buffer length change */
  @tailrec
  private final def insertBetweenChildrenDistinct[T](
    queue: Vector[(Int, IntSlice, Slice[T], Boolean)],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int,
    indexesToTrack: IntBuffer
  ): Int =
    if (queue.isEmpty) offset
    else {

      val (childIndex, childStructure, childValues, insertAfter) = queue.head

      if (childStructure.length == 0)
        insertBetweenChildrenDistinct(queue.tail, structureBuffer, valuesBuffer, offset, indexesToTrack)
      else {

        val (insertIndex, isDistinct) =
          if (structureBuffer.isEmpty) (0, true)
          else if (childIndex >= 0 && childIndex < structureBuffer.length && valuesBuffer(childIndex) == childValues.last)
            (childIndex, false)
          else
            nearestSiblingHavingValue(
              childValues.last,
              Math.max(0, childIndex),
              structureBuffer.length,
              structureBuffer,
              valuesBuffer
            ).map((_, false))
              .getOrElse((if (insertAfter) bottomIndex(childIndex, structureBuffer) else childIndex + 1, true))

        val i = Math.max(0, insertIndex)
        val parent = if (childIndex < 0) 0 else parentIndex(childIndex, structureBuffer)

        val (delta1, updatedQueueTail) = if (isDistinct) {
          if (parent >= 0) {
            structureBuffer.increment(parent)
          }
          structureBuffer.shiftRight(i, 1)
          IndexTracker.trackShiftRight(i, 1, indexesToTrack)
          structureBuffer.update(i, 0)
          valuesBuffer.shiftRight(i, 1)
          valuesBuffer.update(i, childValues.last)
          (1, queue.tail.map(shiftFromWithFlag(i, 1)))
        } else (0, queue.tail)

        val (delta2, updatedQueueTail2) = if (childStructure.length > 1) {
          if (structureBuffer(i) == 0) { // skip checking duplicates and insert remaining tree at once
            val s = treeSize(i, structureBuffer)
            structureBuffer.modify(i, _ + childStructure.last)
            val d = insertSlice(i - s + 1, childStructure.init, childValues.init, structureBuffer, valuesBuffer)
            IndexTracker.trackShiftRight(Math.max(i - s + 1, 0), childStructure.length - 1, indexesToTrack)
            (d, updatedQueueTail.map(shiftFromWithFlag(i - d + 1, d)))
          } else {

            val nextInsertAfter = insertAfter || insertIndex > childIndex

            val childrenInsertIndex =
              if (nextInsertAfter) lastChildIndex(i + delta1, structureBuffer).getOrElse(i + delta1 - 1)
              else i + delta1 - 1

            val nextTail = childrenOf(childStructure, childValues).map {
              case (s, v) => (childrenInsertIndex, s, v, nextInsertAfter)
            }
            (0, nextTail ++: updatedQueueTail)
          }

        } else
          (0, updatedQueueTail)

        insertBetweenChildrenDistinct(
          updatedQueueTail2,
          structureBuffer,
          valuesBuffer,
          offset + delta1 + delta2,
          indexesToTrack
        )
      }
    }

  /** Wraps the tree at index with the new tree, and before/after siblings
    * @return buffers length change */
  final def wrapWithValueAndSiblings[T](
    index: Int,
    value: T,
    beforeSiblings: Iterable[(IntSlice, Slice[T])],
    afterSiblings: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int =
    if (index < 0 || index > structureBuffer.top) 0
    else {
      val newParentIndex = index + 1
      structureBuffer.insert(newParentIndex, 1)
      valuesBuffer.insert(newParentIndex, value)
      val delta1 =
        if (beforeSiblings.isEmpty) 0
        else
          insertBeforeChildren(newParentIndex, beforeSiblings, structureBuffer, valuesBuffer, keepDistinct)
      val delta2 =
        if (afterSiblings.isEmpty) 0
        else insertAfterChildren(newParentIndex + delta1, afterSiblings, structureBuffer, valuesBuffer, keepDistinct)
      delta1 + delta2 + 1
    }

  /** Inserts new children before or after an existing children of the parent index.
    * @param append whether to insert after (if true) or before (if false)
    * @return buffers length change */
  final def insertChildren[T](
    parentIndex: Int,
    newChildren: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    append: Boolean,
    keepDistinct: Boolean
  ): Int =
    if (parentIndex < 0 || parentIndex > structureBuffer.top) 0
    else if (append) insertAfterChildren(parentIndex, newChildren, structureBuffer, valuesBuffer, keepDistinct)
    else insertBeforeChildren(parentIndex, newChildren, structureBuffer, valuesBuffer, keepDistinct)

  /** Inserts new children before or after an existing children of the parent index.
    * @param append whether to insert after (if true) or before (if false)
    * @return buffers length change */
  final def insertChildrenDistinct[T](
    parentIndex: Int,
    childStructure: IntSlice,
    childValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    append: Boolean
  ): Int =
    if (append)
      insertAfterChildrenDistinct(parentIndex, Seq((childStructure, childValues)), structureBuffer, valuesBuffer)
    else insertBeforeChildrenDistinct(parentIndex, Seq((childStructure, childValues)), structureBuffer, valuesBuffer)

  /** Inserts new children before an existing children of the parent index.
    * @return buffers length change */
  final def insertBeforeChildren[T](
    parentIndex: Int,
    newChildren: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int =
    if (parentIndex < 0 || parentIndex > structureBuffer.top) 0
    else if (keepDistinct)
      ArrayTreeFunctions
        .insertBeforeChildrenDistinct(
          parentIndex,
          newChildren,
          structureBuffer,
          valuesBuffer
        )
    else
      newChildren.foldRight(0) {
        case ((structure, values), delta) if structure.length > 0 =>
          structureBuffer.increment(parentIndex + delta)
          ArrayTreeFunctions
            .insertSlice(parentIndex + delta, structure, values, structureBuffer, valuesBuffer) + delta

        case (_, delta) => delta
      }

  /** Inserts new child into the buffers before an existing children with checking for duplicates.
    * New child is eventually inserted before an existing children of its parentIndex.
    * Does nothing for an empty tree.
    * @return buffer length change */
  final def insertBeforeChildDistinct[T](
    parentIndex: Int,
    childStructure: IntSlice,
    childValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    insertBeforeChildrenDistinct(parentIndex, Seq((childStructure, childValues)), structureBuffer, valuesBuffer)

  /** Inserts new children before an existing children into the buffers with checking for duplicates.
    * New child is eventually inserted before an existing children,
    * or merged with the sibling having the same head value.
    * Does nothing for an empty tree.
    * @return buffer length change */
  final def insertBeforeChildrenDistinct[T](
    parentIndex: Int,
    children: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    insertChildrenDistinct(parentIndex, parentIndex, children.iterator, structureBuffer, valuesBuffer, 0)

  /** Inserts new children after an existing children of the parent index.
    * @return buffers length change */
  final def insertAfterChildren[T](
    parentIndex: Int,
    newChildren: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int =
    if (parentIndex < 0 || parentIndex > structureBuffer.top) 0
    else if (keepDistinct)
      ArrayTreeFunctions
        .insertAfterChildrenDistinct(
          parentIndex,
          newChildren,
          structureBuffer,
          valuesBuffer
        )
    else
      newChildren.foldLeft(0) {
        case (delta, (structure, values)) if structure.length > 0 =>
          val bottom = bottomIndex(parentIndex + delta, structureBuffer)
          structureBuffer.increment(parentIndex + delta)
          ArrayTreeFunctions
            .insertSlice(bottom, structure, values, structureBuffer, valuesBuffer) + delta

        case (delta, _) => delta
      }

  /** Inserts new child into the buffers after an existing children with checking for duplicates.
    * New child is eventually inserted after an existing children of its parentIndex,
    * or merged with the sibling having the same head value.
    * Does nothing for an empty tree.
    *
    * @return buffer length change */
  final def insertAfterChildDistinct[T](
    parentIndex: Int,
    childStructure: IntSlice,
    childValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    insertAfterChildrenDistinct(parentIndex, Seq((childStructure, childValues)), structureBuffer, valuesBuffer)

  /** Inserts new children after an existing children into the buffers with checking for duplicates.
    * New child is eventually inserted after an existing children of its parentIndex,
    * or merged with the sibling having the same head value.
    * Does nothing for an empty tree.
    * @return buffer length change */
  final def insertAfterChildrenDistinct[T](
    parentIndex: Int,
    children: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    insertChildrenDistinct(
      parentIndex,
      bottomIndex(parentIndex, structureBuffer),
      children.iterator,
      structureBuffer,
      valuesBuffer,
      0
    )

  /** Inserts new child into the buffers with checking for duplicates.
    * The child is eventually inserted at the specified position,
    * or merged with the sibling having the same head value.
    * Does nothing for an empty tree.
    * @return buffer length change */
  final def insertChildDistinct[T](
    parentIndex: Int,
    insertIndex: Int,
    childStructure: IntSlice,
    childValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    insertChildrenDistinct(
      parentIndex,
      insertIndex,
      Iterator.single((childStructure, childValues)),
      structureBuffer,
      valuesBuffer,
      0
    )

  /** Inserts new children into the buffers with checking for duplicates.
    * Each child is eventually inserted at the specified position,
    * or merged with the sibling having the same head value.
    * Does nothing for an empty tree.
    * @return buffer length change */
  @tailrec
  private final def insertChildrenDistinct[T](
    parentIndex: Int,
    insertIndex: Int,
    queue: Iterator[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    if (queue.isEmpty) offset
    else {
      val (structure, values) = queue.next()

      if (structure.length == 0)
        insertChildrenDistinct(parentIndex, insertIndex, queue, structureBuffer, valuesBuffer, offset)
      else {

        if (parentIndex >= 0) structureBuffer.increment(parentIndex)
        val delta1 = insertSlice(insertIndex, structure, values, structureBuffer, valuesBuffer)

        val duplicatedSiblingIndex =
          if (parentIndex < 0 && structureBuffer.length > 0 && valuesBuffer.head == values.last)
            Some(structureBuffer.top)
          else
            findNearestSiblingHavingValue(
              parentIndex + delta1,
              values.last,
              insertIndex + delta1 - 1,
              structureBuffer,
              valuesBuffer
            )

        val (delta2, nextInsertIndex) = duplicatedSiblingIndex
          .map { recipient =>
            val donor = Math.max(0, insertIndex + delta1 - 1)
            val delta = mergeDeeplyTwoTrees(recipient, donor, structureBuffer, valuesBuffer)
            val index =
              if (recipient < insertIndex) insertIndex + delta + structure.length
              else insertIndex
            (delta, index)
          }
          .getOrElse((0, insertIndex))

        insertChildrenDistinct(
          parentIndex + delta1 + delta2,
          nextInsertIndex,
          queue,
          structureBuffer,
          valuesBuffer,
          offset + delta1 + delta2
        )
      }
    }

  /** Adds d to i if i >= b, returns i otherwise.
    * @param i index
    * @param b base index
    * @param d delta */
  @`inline` final def shiftIfGreaterOrEqualTo(i: Int, b: Int, d: Int): Int = if (i >= b) i + d else i

  /** Transforms tuple (index,structure,content) by adding an offset d to the index if greater or equal to i.
    * @param b base index
    * @param d delta */
  @`inline` final def shiftFrom[T](b: Int, d: Int): ((Int, IntSlice, Slice[T])) => (Int, IntSlice, Slice[T]) = {
    case (i, s, c) => (shiftIfGreaterOrEqualTo(i, b, d), s, c)
  }

  /** Transforms tuple (index,structure,content,flag) by adding an offset d to the index if greater or equal to i.
    * @param b base index
    * @param d delta */
  @`inline` final def shiftFromWithFlag[T](
    b: Int,
    d: Int
  ): ((Int, IntSlice, Slice[T], Boolean)) => (Int, IntSlice, Slice[T], Boolean) = {
    case (i, s, c, f) => (shiftIfGreaterOrEqualTo(i, b, d), s, c, f)
  }

  /** Replaces value with the subtree in the buffers at the given index, allows duplicated children.
    * If the subtree, represented by a tuple of structure and values, is empty then does nothing.
    * @return buffer length change */
  final def expandValueIntoTreeLax[T](
    index: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (index >= 0 &&
        treeStructure.length > 0 &&
        structureBuffer.length > index &&
        structureBuffer.length == valuesBuffer.length) {

      val offset1 = if (index < 0) {
        structureBuffer.shiftRight(0, -index)
        structureBuffer.modifyRange(0, -index, _ => 0)
        valuesBuffer.shiftRight(0, -index)
        -index
      } else 0

      val offset2 = if (treeStructure.length == 1) {
        valuesBuffer(index + offset1) = treeValues.last
        0
      } else {
        val s = treeSize(index + offset1, structureBuffer) - 1
        structureBuffer.modify(index + offset1, _ + treeStructure.last)
        valuesBuffer.update(index + offset1, treeValues.last)
        insertSlice(index + offset1 - s, treeStructure.init, treeValues.init, structureBuffer, valuesBuffer)
      }
      offset1 + offset2
    } else 0

  /** Inserts tree to the buffers at the given index, and keeps children distinct.
    * Does nothing when an empty tree.
    * @return buffer length change */
  final def expandValueIntoTreeDistinct[T](
    index: Int,
    parentIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (index >= 0 &&
        treeStructure.length > 0 &&
        structureBuffer.length > index &&
        structureBuffer.length > parentIndex &&
        structureBuffer.length == valuesBuffer.length) {

      valuesBuffer(index) = treeValues.last

      val (delta1, insertIndex) =
        findNearestSiblingHavingValue(parentIndex, treeValues.last, index, structureBuffer, valuesBuffer) match {
          case Some(i) =>
            val track = IntBuffer(i)
            val delta = mergeDeeplyTwoTrees(i, index, structureBuffer, valuesBuffer, track)
            (delta, track.head)

          case _ => (0, index)
        }

      val delta2 =
        if (treeStructure.length == 1) 0
        else
          insertChildrenDistinct(
            insertIndex,
            bottomIndex(insertIndex, structureBuffer),
            childrenOf(treeStructure, treeValues),
            structureBuffer,
            valuesBuffer,
            0
          )

      delta1 + delta2

    } else 0

  /** Ensures that a child's value at index is distinct, otherwise merges down with the nearest sibling
    * on the right (above, preferred) or on the left (below). */
  final def ensureChildDistinct[T](
    index: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer = IntBuffer.empty
  ): Int =
    if (index < 0 || index >= structureBuffer.length) 0
    else
      nearestSiblingHavingValue(valuesBuffer(index), index, structureBuffer.length, structureBuffer, valuesBuffer) match {
        case None => 0
        case Some(duplicateIndex) =>
          val (delta1, resultIndex) = ArrayTreeFunctions.mergeShallowTwoTrees(
            Math.max(index, duplicateIndex),
            Math.min(index, duplicateIndex),
            structureBuffer,
            valuesBuffer,
            indexesToTrack
          )
          val delta2 =
            makeChildrenDistinct(resultIndex, pullUp = true, structureBuffer, valuesBuffer, indexesToTrack)
          delta1 + delta2
      }

  /** Merges duplicated children of the parent index, merging down recursively if necessary.
    * @param pullUp if true, higher index value will be a recipient
    * @note This function modifies the buffers and is NOT multi-thread safe.
    * @return buffers length change */
  final def makeChildrenDistinct[T](
    parentIndex: Int,
    pullUp: Boolean,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer = IntBuffer.empty
  ): Int =
    makeChildrenDistinct(IntBuffer(parentIndex), pullUp, 0, structureBuffer, valuesBuffer, indexesToTrack)

  /** Merges duplicated children of the nodes in the queue, merging down recursively if necessary.
    * @return buffers length change */
  @tailrec
  private final def makeChildrenDistinct[T](
    queue: IntBuffer,
    pullUp: Boolean,
    delta: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer
  ): Int =
    if (queue.isEmpty) delta
    else {
      val parentIndex = queue.head
      val childrenPairs = childrenIndexes(parentIndex, structureBuffer).asSlice.map(i => (i, valuesBuffer(i)))
      findFirstDuplicatePair[(Int, T), T](childrenPairs, _._2, rightToLeft = pullUp) match {
        case None =>
          makeChildrenDistinct(queue.tail, pullUp, delta, structureBuffer, valuesBuffer, indexesToTrack)

        case Some(((leftIndex, _), (rightIndex, _))) =>
          val (delta2, index) =
            mergeShallowTwoTrees(rightIndex, leftIndex, structureBuffer, valuesBuffer, queue, indexesToTrack)
          makeChildrenDistinct(
            queue.push(index),
            pullUp,
            delta + delta2,
            structureBuffer,
            valuesBuffer,
            indexesToTrack
          )
      }
    }

  /** Finds first pair of values having a duplicated key. */
  final def findFirstDuplicatePair[T, K](
    values: Slice[T],
    keyOf: T => K,
    rightToLeft: Boolean = false
  ): Option[(T, T)] = {
    val map = mutable.Map[K, T]()
    (if (rightToLeft) values.reverseIterator else values.iterator)
      .find { (value: T) =>
        val key = keyOf(value)
        map.contains(key) || { map.update(key, value); false }
      }
      .map(value => (map(keyOf(value)), value))
  }

  /** Merges deeply two trees, recipient and donor, if indexes are in a tree range and both indexes distinct.
    * @note This function modifies the buffers and is NOT multi-thread safe.
    * @return buffers length change
    */
  final def mergeDeeplyTwoTrees[T](
    recipientIndex: Int,
    donorIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer = IntBuffer.empty
  ): Int = {
    val (delta1, modifiedRecipient) =
      mergeShallowTwoTrees(recipientIndex, donorIndex, structureBuffer, valuesBuffer, indexesToTrack)
    val delta2 = makeChildrenDistinct(
      modifiedRecipient,
      recipientIndex > donorIndex,
      structureBuffer,
      valuesBuffer,
      indexesToTrack
    )
    delta1 + delta2
  }

  /** Merges shallow two trees, recipient and donor, if indexes are in a tree range and both indexes distinct.
    * - Children of donor are joined with children of recipient.
    * - Donor top node is removed.
    * - If the recipient is a subtree of a donor, it is moved first outside
    *   and does not replicate itself as a nested child.
    *
    * @note This function modifies the buffers and is NOT multi-thread safe.
    * @return a tuple of (tree size change, resulting recipient index)
    */
  final def mergeShallowTwoTrees[T](
    recipientIndex: Int,
    donorIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToTrack: IntBuffer*
  ): (Int, Int) =
    if (recipientIndex < 0 || recipientIndex >= structureBuffer.length || donorIndex < 0 || donorIndex >= structureBuffer.length || recipientIndex == donorIndex)
      (0, recipientIndex)
    else {
      val donorParentIndex = parentIndex(donorIndex, structureBuffer)
      val recipientSize = treeSize(recipientIndex, structureBuffer)
      val donorSize = treeSize(donorIndex, structureBuffer)
      val recipientIsDonorSubtree = recipientIndex < donorIndex && recipientIndex > donorIndex - donorSize

      if (recipientIsDonorSubtree) {
        // decrement recipient's parent children count
        val recipientParentIndex = parentIndex(recipientIndex, structureBuffer)
        structureBuffer.decrement(recipientParentIndex)
      }
      // add a donor node's children count to the remaining sibling node's children count
      structureBuffer.modify(recipientIndex, _ + structureBuffer(donorIndex))
      // decrement donor's parent children count
      if (donorParentIndex >= 0) {
        structureBuffer.decrement(donorParentIndex)
      }
      // remove the top donor node
      structureBuffer.shiftLeft(donorIndex + 1, 1)
      valuesBuffer.shiftLeft(donorIndex + 1, 1)
      indexesToTrack.foreach(trackShiftLeft(donorIndex + 1, 1, _))

      val resultingRecipientIndex = if (recipientIsDonorSubtree) {
        // relocate recipient tree outside of a donor tree
        val gap = donorIndex - recipientIndex - 1
        structureBuffer.moveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap)
        valuesBuffer.moveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap)
        indexesToTrack.foreach(trackMoveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap, _))
        recipientIndex + gap
      } else {
        // relocate donor tree next to the recipient tree
        if (donorIndex < recipientIndex - recipientSize) {
          val gap = recipientIndex - recipientSize - donorIndex
          structureBuffer.moveRangeRight(donorIndex - donorSize + 1, donorIndex, gap)
          valuesBuffer.moveRangeRight(donorIndex - donorSize + 1, donorIndex, gap)
          indexesToTrack.foreach(trackMoveRangeRight(donorIndex - donorSize + 1, donorIndex, gap, _))
          recipientIndex - 1
        } else if (recipientIndex <= donorIndex - donorSize) {
          val gap = donorIndex - donorSize - recipientIndex + 1 /* + recipientSize*/
          structureBuffer.moveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap)
          valuesBuffer.moveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap)
          indexesToTrack.foreach(trackMoveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap, _))
          recipientIndex + donorSize - 1
        } else {
          recipientIndex - 1
        }
      }

      (-1, resultingRecipientIndex)
    }
}
