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

/** Lazy slice of the array. */
final class Slice[A] private (fromIndex: Int, toIndex: Int, array: Array[A]) extends (Int => A) {

  val length: Int = toIndex - fromIndex

  def apply(index: Int): A = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `apply` index in the interval [0,$length), but was $index.")
    array.apply(fromIndex + index)
  }

  def update(index: Int, value: A): Slice[A] = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `update` index in the interval [0,$length), but was $index.")
    array.update(fromIndex + index, value)
    this
  }

  def map[B](f: A => B)(implicit tagA: ClassTag[A], tagB: ClassTag[B]): Slice[B] = Slice.of(toArray.map(f))

  def count(pred: A => Boolean): Int = {
    var a = 0
    var i = fromIndex
    while (i < toIndex) {
      if (pred(array(i))) a = a + 1
      i = i + 1
    }
    a
  }

  def isEmpty: Boolean = length <= 0

  def slice(from: Int, to: Int): Slice[A] = {
    val t = fit(0, to, length)
    val f = fit(0, from, t)
    if (f == 0 && t == length) this
    else new Slice[A](fromIndex + f, fromIndex + t, array)
  }

  private def fit(lower: Int, value: Int, upper: Int): Int =
    Math.min(Math.max(lower, value), upper)

  def take(n: Int): Slice[A] = {
    assert(n >= 0, "take parameter must be equal or greater to zero")
    new Slice[A](fromIndex, Math.min(fromIndex + n, toIndex), array)
  }

  def takeRight(n: Int): Slice[A] = {
    assert(n >= 0, "takeRight parameter must be equal or greater to zero")
    new Slice[A](Math.max(toIndex - n, fromIndex), toIndex, array)
  }

  def drop(n: Int): Slice[A] = {
    assert(n >= 0, "drop parameter must be equal or greater to zero")
    new Slice[A](Math.min(fromIndex + n, toIndex), toIndex, array)
  }

  def dropRight(n: Int): Slice[A] = {
    assert(n >= 0, "dropRight parameter must be equal or greater to zero")
    new Slice[A](fromIndex, Math.max(toIndex - n, fromIndex), array)
  }

  def iterator: Iterator[A] = new Iterator[A] {

    var i = fromIndex

    override def hasNext: Boolean = i < toIndex

    override def next(): A = {
      val value = array(i)
      i = i + 1
      value
    }
  }

  def reverseIterator: Iterator[A] = new Iterator[A] {

    var i = toIndex - 1

    override def hasNext: Boolean = i >= fromIndex

    override def next(): A = {
      val value = array(i)
      i = i - 1
      value
    }
  }

  def reverseIterator(pred: A => Boolean): Iterator[A] = new Iterator[A] {

    var i = toIndex - 1

    if (i >= fromIndex) seekNext

    override def hasNext: Boolean = i >= fromIndex

    override def next(): A = {
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

  def toArray(implicit tag: ClassTag[A]): Array[A] = {
    val newArray = new Array[A](length)
    Array.copy(array, fromIndex, newArray, 0, length)
    newArray
  }

  def toList: List[A] = iterator.toList

  override def toString: String =
    iterator.take(Math.min(20, length)).mkString("Slice(", ",", if (length > 20) ", ... )" else ")")

  override def equals(obj: Any): Boolean = obj match {
    case other: Slice[A] =>
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

object Slice {

  def apply(is: Int*): Slice[Int] = Slice.ofInt(Array(is: _*))

  def apply[T: ClassTag](is: T*): Slice[T] = Slice.of(Array(is: _*))

  def of[T](array: Array[T]): Slice[T] = new Slice[T](0, array.length, array)

  def of[T](array: Array[T], from: Int, to: Int): Slice[T] = {
    assert(from >= 0, "When creating a Slice, parameter `from` must be greater or equal to zero.")
    assert(to <= array.length, "When creating a Slice, parameter `to` must be lower or equal to the array length.")
    assert(from <= to, "When creating a Slice, parameter `from` must be lower or equal to the parameter `to`.")
    new Slice[T](from, to, array)
  }

  def ofInt(array: Array[Int]): Slice[Int] = new Slice[Int](0, array.length, array)

  def ofInt(array: Array[Int], from: Int, to: Int): Slice[Int] = {
    assert(from >= 0, "When creating a Slice, parameter `from` must be greater or equal to 0.")
    assert(to <= array.length, "When creating a Slice, parameter `to` must be lower or equal to the array length.")
    assert(from <= to, "When creating a Slice, parameter `from` must be lower or equal to `to`.")
    new Slice[Int](from, to, array)
  }

}
