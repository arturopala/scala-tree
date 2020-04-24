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

/** Growable, mutable array of integers.
  * Unlike [[ArrayBuffer]] allows to address elements outside the range. */
final class IntBuffer(initialSize: Int = 8) extends ArrayBufferLike[Int] {

  private var _array = new Array[Int](initialSize)

  protected override def array: Array[Int] = _array

  /** Returns value at the given index or 0 if out of scope. */
  @`inline` def apply(index: Int): Int =
    if (index < 0 || index >= _array.length) 0
    else _array(index)

  override protected def ensureIndex(index: Int): Unit =
    if (index >= _array.length) {
      val newArray: Array[Int] = new Array(Math.max(_array.length * 2, index + 1))
      java.lang.System.arraycopy(_array, 0, newArray, 0, _array.length)
      _array = newArray
    }

  /** Increments the value at index.s */
  def increment(index: Int): this.type = {
    update(index, apply(index) + 1)
    this
  }

  /** Returns copy of the underlying array trimmed to length. */
  def toArray: Array[Int] = java.util.Arrays.copyOf(_array, length)

  /** Wraps underlying array as a Slice. */
  def toSlice: IntSlice = IntSlice.of(_array, 0, length)

  /** Copy values directly from IntSlice's array into the buffer array. */
  def copyFrom(index: Int, slice: IntSlice): Unit = {
    ensureIndex(index + slice.length - 1)
    java.lang.System.arraycopy(slice.array, slice.fromIndex, _array, index, slice.length)
  }

}

/** IntBuffer factory. */
object IntBuffer {

  /** Create buffer with initial values. */
  def apply(elems: Int*): IntBuffer = new IntBuffer(elems.size).appendArray(elems.toArray)

  /** Create buffer from an array copy. */
  def apply(array: Array[Int]): IntBuffer = new IntBuffer(array.length).appendArray(array)

  /** An empty buffer. */
  def empty = new IntBuffer(0)
}
