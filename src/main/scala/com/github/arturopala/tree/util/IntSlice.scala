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
import scala.reflect.ClassTag

/** Lazy slice of the array of integers. */
final class IntSlice private (fromIndex: Int, toIndex: Int, array: Array[Int]) extends (Int => Int) {

  val length: Int = toIndex - fromIndex

  def apply(index: Int): Int = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `apply` index in the interval [0,$length), but was $index.")
    array.apply(fromIndex + index)
  }

  def update(index: Int, value: Int): IntSlice = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `update` index in the interval [0,$length), but was $index.")
    val modified = toArray
    modified.update(index, value)
    new IntSlice(0, length, modified)
  }

  def map[B](f: Int => B)(implicit tag: ClassTag[B]): Slice[B] = Slice.of[B, Int](fromIndex, toIndex, array, f)

  def count(pred: Int => Boolean): Int = {
    var a = 0
    var i = fromIndex
    while (i < toIndex) {
      if (pred(array(i))) a = a + 1
      i = i + 1
    }
    a
  }

  def isEmpty: Boolean = length <= 0

  def slice(from: Int, to: Int): IntSlice = {
    val t = fit(0, to, length)
    val f = fit(0, from, t)
    if (f == 0 && t == length) this
    else new IntSlice(fromIndex + f, fromIndex + t, array)
  }

  private def fit(lower: Int, value: Int, upper: Int): Int =
    Math.min(Math.max(lower, value), upper)

  def take(n: Int): IntSlice = {
    assert(n >= 0, "take parameter must be equal or greater to zero")
    new IntSlice(fromIndex, Math.min(fromIndex + n, toIndex), array)
  }

  def takeRight(n: Int): IntSlice = {
    assert(n >= 0, "takeRight parameter must be equal or greater to zero")
    new IntSlice(Math.max(toIndex - n, fromIndex), toIndex, array)
  }

  def drop(n: Int): IntSlice = {
    assert(n >= 0, "drop parameter must be equal or greater to zero")
    new IntSlice(Math.min(fromIndex + n, toIndex), toIndex, array)
  }

  def dropRight(n: Int): IntSlice = {
    assert(n >= 0, "dropRight parameter must be equal or greater to zero")
    new IntSlice(fromIndex, Math.max(toIndex - n, fromIndex), array)
  }

  def iterator: Iterator[Int] = new Iterator[Int] {

    var i = fromIndex

    override def hasNext: Boolean = i < toIndex

    override def next(): Int = {
      val value = array(i)
      i = i + 1
      value
    }
  }

  def reverseIterator: Iterator[Int] = new Iterator[Int] {

    var i = toIndex - 1

    override def hasNext: Boolean = i >= fromIndex

    override def next(): Int = {
      val value = array(i)
      i = i - 1
      value
    }
  }

  def reverseIterator(pred: Int => Boolean): Iterator[Int] = new Iterator[Int] {

    var i = toIndex - 1

    if (i >= fromIndex) seekNext

    override def hasNext: Boolean = i >= fromIndex

    override def next(): Int = {
      val value = array(i)
      i = i - 1
      seekNext
      value
    }

    def seekNext: Unit = {
      var v = array(i)
      while (!pred(v) && i >= fromIndex) {
        i = i - 1
        if (i > fromIndex) v = array(i)
      }
    }
  }

  def toArray(implicit tag: ClassTag[Int]): Array[Int] = {
    val newArray = new Array[Int](length)
    Array.copy(array, fromIndex, newArray, 0, length)
    newArray
  }

  def toList: List[Int] = iterator.toList

  override def toString: String =
    iterator.take(Math.min(20, length)).mkString("Slice(", ",", if (length > 20) ", ... )" else ")")

  override def equals(obj: Any): Boolean = obj match {
    case other: IntSlice =>
      this.length == other.length &&
        Compare.sameElements(this.iterator, other.iterator)

    case _ => false
  }

  override def hashCode(): Int = {
    var hash = 17
    hash = hash * 31 + this.length
    for (i <- fromIndex to toIndex by (length / 7)) {
      hash = hash * 31 + array(i).hashCode()
    }
    hash
  }
}

object IntSlice {

  def apply(is: Int*): IntSlice = IntSlice.of(Array(is: _*))

  def of(array: Array[Int]): IntSlice = new IntSlice(0, array.length, array)

  def of(array: Array[Int], from: Int, to: Int): IntSlice = {
    assert(from >= 0, "When creating a Slice, parameter `from` must be greater or equal to 0.")
    assert(to <= array.length, "When creating a Slice, parameter `to` must be lower or equal to the array length.")
    assert(from <= to, "When creating a Slice, parameter `from` must be lower or equal to `to`.")
    new IntSlice(from, to, array)
  }

  def empty: IntSlice = IntSlice()

}
