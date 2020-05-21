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

import scala.annotation.tailrec
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
    index - treeSize(index, treeStructure) + 1

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

  /** Lists indexes of the children values of the parent node, if any. */
  final def childrenIndexes(parentIndex: Int, treeStructure: Int => Int): IntSlice = {
    val result = IntBuffer()
    if (parentIndex >= 0) {
      val numberOfChildren = treeStructure(parentIndex)
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
    }
    result.asSlice
  }

  /** Returns a list of children of a tree represented by the structure and content slices. */
  @`inline` final def childrenOf[T](treeStructure: IntSlice, treeValues: Slice[T]): Iterator[(IntSlice, Slice[T])] =
    childrenIndexes(treeStructure.length - 1, treeStructure)
      .map(subtreeAt(_, treeStructure, treeValues))
      .iterator(_._1.length > 0)

  /** Returns a reversed list of children of a tree represented by the structure and content slices. */
  @`inline` final def reversedChildrenOf[T](
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Iterator[(IntSlice, Slice[T])] =
    childrenIndexes(treeStructure.length - 1, treeStructure)
      .map(subtreeAt(_, treeStructure, treeValues))
      .reverseIterator(_._1.length > 0)

  /** Returns a structure and content slices representing a subtree rooted at the given index. */
  final def subtreeAt[T](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): (IntSlice, Slice[T]) = {
    val size = treeSize(index, treeStructure)
    val structure = treeStructure.slice(index - size + 1, index + 1)
    val values = treeValues.slice(index - size + 1, index + 1)
    (structure, values)
  }

  /** Appends children indexes to the buffer, starting from the given position.
    * @return number of children appended
    */
  final def writeChildrenIndexes(
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

  /** Finds an index of a sibling of childIndex having some value. */
  final def siblingOfValue[T](
    value: T,
    childIndex: Int,
    size: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    val parent = parentIndex(childIndex, size, treeStructure)
    if (parent < 0) None
    else {
      val children = childrenIndexesFor(value, parent, treeStructure, treeValues)
      if (children.isEmpty) None
      else {
        var result: Option[Int] = None
        var i = 0
        while (i >= 0 && i < children.length) {
          if (children(i) == childIndex) {
            result =
              if (i > 0) Some(children(i - 1))
              else if (i < children.length - 1) Some(children(i + 1))
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
  }

  /** Looks for Some index of the child node holding the given value, or None.
    * @return the rightmost index if many */
  final def leftmostIndexOfChildValue[T](
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
    * @return the leftmost index if many */
  final def rightmostIndexOfChildValue[T](
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
    * @note tree structure as returned by [[com.github.arturopala.tree.Tree.toArrays]] */
  final def childrenIndexesFor[T](
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
  final def nodeIndexIteratorWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[Int] =
    new Iterator[Int] {

      var hasNext: Boolean = false
      var i: Int = startIndex

      val counters = new IntBuffer(8)
      val indexes = new IntBuffer(8)

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      override def next(): Int =
        if (hasNext) {
          val result = i
          seekNext(true)
          result
        } else throw new NoSuchElementException

      def seekNext(check: Boolean): Unit =
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
              writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
            }
          }
        }
    }

  /** Iterates over tree's branches as index lists, depth first. */
  final def branchesIndexListIterator(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[IntSlice] =
    new Iterator[IntSlice] {

      var hasNext: Boolean = false
      var array: IntSlice = IntSlice.empty

      val counters = new IntBuffer(8)
      val indexes = new IntBuffer(8)

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

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
            if (c == 0 || counters.length >= maxDepth - 1) {
              array = BranchIteratorUtils.readBranch(counters, indexes).push(i).asSlice
              hasNext = true
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
              seekNext(false)
            }
          }
        }
    }

  private object BranchIteratorUtils {

    def readBranch(counters: IntBuffer, indexes: IntBuffer): IntBuffer = {
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

    def retract(counters: IntBuffer): Unit =
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
            writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
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
    calculateHeight(treeStructure.length - 1, treeStructure)

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
            writeChildrenIndexes(ci, treeStructure, children, 0)
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
  def insertValue[T](index: Int, value: T, structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Int = {
    structureBuffer.increment(index)
    structureBuffer.shiftRight(index, 1)
    structureBuffer.update(index, 0)
    valuesBuffer.shiftRight(index, 1)
    valuesBuffer.update(index, value)
    1
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
  final def insert[T](
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
  final def insert[T](
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
      rightmostIndexOfChildValue(value, parentIndex, structureBuffer.length, structureBuffer, valuesBuffer) match {
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
          structureBuffer.insertFromIterator(insertIndex + 1, delta - 1, Iterator.continually(1))
          delta
      }
    } else offset

  /** Inserts children into the buffers with checking for duplicates.
    * Each child is inserted on the right side (below) of existing children of its parentIndex.
    * Does for an empty tree.
    * @param queue list of (parentIndex, tree to insert)
    * @return buffer length change */
  @tailrec
  final def insertRightSubtreeListDistinct[T](
    queue: Vector[(Int, IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    if (queue.isEmpty) offset
    else {
      val (parentIndex, structure, values) = queue.head
      val tail = queue.tail

      if (structure.length == 0) insertRightSubtreeListDistinct(queue.tail, structureBuffer, valuesBuffer, offset)
      else {
        val (insertIndex, isDistinct) =
          rightmostIndexOfChildValue(values.last, parentIndex, structureBuffer.length, structureBuffer, valuesBuffer)
            .map((_, false))
            .getOrElse((Math.max(0, parentIndex - treeSize(parentIndex, structureBuffer) + 1), true))

        val (delta1, updatedTail) = if (isDistinct) {
          structureBuffer.shiftRight(insertIndex, 1)
          structureBuffer.update(insertIndex, 0)
          if (parentIndex + 1 > insertIndex) {
            structureBuffer.increment(parentIndex + 1)
          }
          valuesBuffer.shiftRight(insertIndex, 1)
          valuesBuffer.update(insertIndex, values.last)
          (1, tail.map(shiftFrom(insertIndex, 1)))
        } else (0, tail)

        val (delta2, updatedQueue) = if (structure.length > 1) {
          if (structureBuffer(insertIndex) == 0) { // skip checking duplicates and insert remaining tree at once
            val s = treeSize(insertIndex, structureBuffer)
            structureBuffer.modify(insertIndex, _ + structure.last)
            val d = insert(insertIndex - s + 1, structure.init, values.init, structureBuffer, valuesBuffer)
            (d, updatedTail.map(shiftFrom(insertIndex - d + 1, d)))
          } else {
            val nextTail = childrenOf(structure, values).map { case (s, v) => (insertIndex, s, v) }
            (0, nextTail ++: updatedTail)
          }

        } else
          (0, updatedTail)

        insertRightSubtreeListDistinct(updatedQueue, structureBuffer, valuesBuffer, offset + delta1 + delta2)
      }
    }

  /** Inserts children into the buffers with checking for duplicates.
    * Each child is inserted on the left side (above) of existing children of its parentIndex.
    * Does nothing for an empty tree.
    * @param queue list of (parentIndex, tree to insert)
    * @return buffer length change */
  @tailrec
  final def insertLeftSubtreeListDistinct[T](
    queue: Vector[(Int, IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    if (queue.isEmpty) offset
    else {
      val (parentIndex, structure, values) = queue.head
      val tail = queue.tail

      if (structure.length == 0) insertLeftSubtreeListDistinct(tail, structureBuffer, valuesBuffer, offset)
      else {
        val (insertIndex, isDistinct) =
          leftmostIndexOfChildValue(values.last, parentIndex, structureBuffer.length, structureBuffer, valuesBuffer)
            .map((_, false))
            .getOrElse((Math.max(0, parentIndex), true))

        val (delta1, updatedTail) = if (isDistinct) {
          structureBuffer.shiftRight(insertIndex, 1)
          structureBuffer.update(insertIndex, 0)
          if (parentIndex + 1 > insertIndex) {
            structureBuffer.increment(parentIndex + 1)
          }
          valuesBuffer.shiftRight(insertIndex, 1)
          valuesBuffer.update(insertIndex, values.last)
          (1, tail.map(shiftFrom(insertIndex, 1)))
        } else (0, tail)

        val (delta2, updatedQueue) = if (structure.length > 1) {
          if (structureBuffer(insertIndex) == 0) { // skip checking duplicates and insert remaining tree at once
            val s = treeSize(insertIndex, structureBuffer)
            structureBuffer.modify(insertIndex, _ + structure.last)
            val d = insert(insertIndex - s + 1, structure.init, values.init, structureBuffer, valuesBuffer)
            (d, updatedTail.map(shiftFrom(insertIndex - d + 1, d)))
          } else {
            val nextTail = reversedChildrenOf(structure, values).map { case (s, v) => (insertIndex, s, v) }
            (0, nextTail ++: updatedTail)
          }
        } else
          (0, updatedTail)

        insertLeftSubtreeListDistinct(updatedQueue, structureBuffer, valuesBuffer, offset + delta1 + delta2)
      }
    }

  /** Adds d to i if i >= b, returns i otherwise. */
  @`inline` final def shiftIfGreaterOrEqualTo(i: Int, b: Int, d: Int): Int = if (i >= b) i + d else i

  /** Transforms (index,structure,content) by adding an offset d to the index if greater or equal to i. */
  @`inline` final def shiftFrom[T](i: Int, d: Int): ((Int, IntSlice, Slice[T])) => (Int, IntSlice, Slice[T]) = {
    case (p, s, c) => (shiftIfGreaterOrEqualTo(p, i, d), s, c)
  }

  /** Replaces value with the subtree in the buffers at the given index, allows duplicated children.
    * If the subtree, represented by a tuple of structure and values, is empty then does nothing.
    * @return buffer length change */
  final def expandValueIntoTree[T](
    treeStructure: IntSlice,
    treeValues: Slice[T],
    index: Int,
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
        insert(index + offset1 - s, treeStructure.init, treeValues.init, structureBuffer, valuesBuffer)
      }
      offset1 + offset2
    } else 0

  /** Inserts tree to the buffers at the given index, and keeps children distinct.
    * Does nothing when an empty tree.
    * @return buffer length change */
  final def expandValueIntoTreeDistinct[T](
    treeStructure: IntSlice,
    treeValues: Slice[T],
    index: Int,
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (index >= 0 &&
        treeStructure.length > 0 &&
        structureBuffer.length > index &&
        structureBuffer.length > parentIndex &&
        structureBuffer.length == valuesBuffer.length) {

      val (insertIndex, hasDuplicate) =
        rightmostIndexOfChildValue(treeValues.last, parentIndex, structureBuffer.length, structureBuffer, valuesBuffer)
          .map(i => (i, i != index))
          .getOrElse((index, false))

      valuesBuffer(insertIndex) = treeValues.last

      val delta1 =
        if (treeStructure.length == 1) 0
        else {
          val queue = childrenOf(treeStructure, treeValues)
            .map {
              case (structure, values) => (insertIndex, structure, values)
            }
          insertRightSubtreeListDistinct(queue.toVector, structureBuffer, valuesBuffer, 0)
        }

      val delta2 = if (hasDuplicate) {
        val index2 = if (insertIndex < index) index + delta1 else index
        removeValue(index2, parentIndex + delta1, structureBuffer, valuesBuffer)
      } else 0

      delta1 + delta2

    } else 0

  /** Inserts tree to the buffers at the given index, and keeps children distinct.
    * Does nothing when an empty tree. Uses unsafe recursion.
    * @return buffers length change */
  final def insertTreeDistinctUnsafe[T](
    treeStructure: IntSlice,
    treeValues: Slice[T],
    index: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (treeStructure.length == 0) 0
    else {
      valuesBuffer(index) = treeValues.last
      childrenOf(treeStructure, treeValues).foldLeft(0) {
        case (offset, (structure, values)) =>
          val bufferIndex = index + offset
          offset + (leftmostIndexOfChildValue(
            treeValues.last,
            bufferIndex,
            structureBuffer.length,
            structureBuffer,
            valuesBuffer
          ) match {
            case None =>
              val s = treeSize(bufferIndex, structureBuffer) - 1
              structureBuffer.increment(bufferIndex)
              insert(bufferIndex - s, treeStructure, treeValues, structureBuffer, valuesBuffer)

            case Some(i) => insertTreeDistinctUnsafe(structure, values, i, structureBuffer, valuesBuffer)
          })
      }
    }

  /** Merges duplicated children of the node at parent's index, merging down recursively if necessary.
    * @note This function modifies the buffers and is NOT multi-thread safe.
    * @return buffers length change */
  final def makeChildrenDistinct[T](parentIndex: Int, structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Int =
    makeChildrenDistinct(IntBuffer(parentIndex), 0, structureBuffer, valuesBuffer)

  @tailrec
  /** Merges duplicated children of the nodes in the queue, merging down recursively if necessary.
    * @return buffers length change */
  private def makeChildrenDistinct[T](
    queue: IntBuffer,
    delta: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (queue.isEmpty) delta
    else {
      val parentIndex = queue.head
      val childrenPairs = childrenIndexes(parentIndex, structureBuffer).map(i => (i, valuesBuffer(i)))
      findFirstDuplicatePair[(Int, T), T](childrenPairs, _._2, rightToLeft = true) match {
        case None =>
          makeChildrenDistinct(queue.tail, delta, structureBuffer, valuesBuffer)

        case Some(((leftIndex, _), (rightIndex, _))) =>
          val (delta2, index) = mergeTwoTrees(rightIndex, leftIndex, structureBuffer, valuesBuffer, queue)
          makeChildrenDistinct(queue.push(index), delta + delta2, structureBuffer, valuesBuffer)
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

  /** Merges two trees, recipient and donor, if indexes are in a tree range and both distinct.
    * - Children of donor are joined with children of recipient.
    * - Donor top node is removed.
    * - If the recipient is a subtree of a donor, it is moved first outside
    *   and does not replicate itself as a nested child.
    *
    * @note This function modifies the buffers and is NOT multi-thread safe.
    * @return a tuple of (tree size change, resulting recipient index)
    */
  final def mergeTwoTrees[T](
    recipientIndex: Int,
    donorIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    indexesToSync: IntBuffer = IntBuffer.empty
  ): (Int, Int) =
    if (recipientIndex < 0 || recipientIndex >= structureBuffer.length || donorIndex < 0 || donorIndex >= structureBuffer.length || recipientIndex == donorIndex)
      (0, recipientIndex)
    else {
      val donorParentIndex = parentIndex(donorIndex, structureBuffer.length, structureBuffer)
      val recipientSize = treeSize(recipientIndex, structureBuffer)
      val donorSize = treeSize(donorIndex, structureBuffer)
      val recipientIsDonorSubtree = recipientIndex < donorIndex && recipientIndex > donorIndex - donorSize

      if (recipientIsDonorSubtree) {
        // decrement recipient's parent children count
        val recipientParentIndex = parentIndex(recipientIndex, structureBuffer.length, structureBuffer)
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
      trackShiftLeft(donorIndex + 1, 1, indexesToSync)

      val resultingRecipientIndex = if (recipientIsDonorSubtree) {
        // relocate recipient tree outside of a donor tree
        val gap = donorIndex - recipientIndex - 1
        structureBuffer.moveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap)
        valuesBuffer.moveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap)
        trackMoveRangeRight(recipientIndex - recipientSize + 1, recipientIndex + 1, gap, indexesToSync)
        recipientIndex + gap
      } else {
        // relocate donor tree next to the recipient tree
        if (donorIndex < recipientIndex - recipientSize) {
          val gap = recipientIndex - recipientSize - donorIndex
          structureBuffer.moveRangeRight(donorIndex - donorSize + 1, donorIndex, gap)
          valuesBuffer.moveRangeRight(donorIndex - donorSize + 1, donorIndex, gap)
          trackMoveRangeRight(donorIndex - donorSize + 1, donorIndex, gap, indexesToSync)
          recipientIndex - 1
        } else if (recipientIndex <= donorIndex - donorSize) {
          val gap = donorIndex - donorSize - recipientIndex + recipientSize
          structureBuffer.moveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap)
          valuesBuffer.moveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap)
          trackMoveRangeLeft(donorIndex - donorSize + 1, donorIndex, gap, indexesToSync)
          recipientIndex + donorSize - 1
        } else {
          recipientIndex - 1
        }
      }

      (-1, resultingRecipientIndex)
    }

  /** Inserts children on the left side (above) of existing children of the parent index.
    * @return buffers length change */
  final def insertLeftChildren[T](
    parentIndex: Int,
    children: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int =
    if (parentIndex < 0 || parentIndex > structureBuffer.top) 0
    else
      children.foldRight(0) {
        case ((structure, values), delta) =>
          if (keepDistinct)
            ArrayTreeFunctions
              .insertLeftSubtreeListDistinct(
                Vector((parentIndex + delta, structure, values)),
                structureBuffer,
                valuesBuffer,
                0
              )
          else {
            structureBuffer.increment(parentIndex + delta)
            ArrayTreeFunctions
              .insert(parentIndex + delta, structure, values, structureBuffer, valuesBuffer)
          } + delta
      }

  /** Inserts children on the right side (below) of existing children of the parent index.
    * @return buffers length change */
  final def insertRightChildren[T](
    parentIndex: Int,
    children: Iterable[(IntSlice, Slice[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    keepDistinct: Boolean
  ): Int =
    if (parentIndex < 0 || parentIndex > structureBuffer.top) 0
    else
      children.foldLeft(0) {
        case (delta, (structure, values)) =>
          if (keepDistinct)
            ArrayTreeFunctions
              .insertRightSubtreeListDistinct(
                Vector((parentIndex + delta, structure, values)),
                structureBuffer,
                valuesBuffer,
                0
              )
          else {
            val bottom = bottomIndex(parentIndex + delta, structureBuffer)
            structureBuffer.increment(parentIndex + delta)
            ArrayTreeFunctions
              .insert(bottom, structure, values, structureBuffer, valuesBuffer)
          } + delta
      }

  /** Wraps the tree at index with the new tree, and left/right siblings
    * @return buffers length change */
  final def wrapWithValueAndSiblings[T](
    index: Int,
    value: T,
    leftSiblings: Iterable[(IntSlice, Slice[T])],
    rightSiblings: Iterable[(IntSlice, Slice[T])],
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
        insertLeftChildren(newParentIndex, leftSiblings, structureBuffer, valuesBuffer, keepDistinct)
      val delta2 =
        insertRightChildren(newParentIndex + delta1, rightSiblings, structureBuffer, valuesBuffer, keepDistinct)
      delta1 + delta2 + 1
    }
}