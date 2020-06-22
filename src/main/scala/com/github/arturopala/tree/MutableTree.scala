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

import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.internal.{ArrayTreeFunctions, ArrayTreeLike, Transformer}

import scala.language.implicitConversions

/** Mutable variant of the [[Tree]].
  * Operations which does not change the type of Tree happens in-place,
  * i.e. mutate the underlying data structures, but might return different wrapper instance.
  * */
sealed trait MutableTree[+T] extends TreeLike[MutableTree, T] {

  /** Freezes and returns as immutable [[Tree]].
    * @note Makes a copy of an underlying data structures.
    * @group optimization
    */
  def immutable: Tree[T]

  final override def toString: String =
    if (isEmpty) "MutableTree()"
    else {
      if (size < 50)
        s"MutableTree(${Tree.stringify(headOption.get)}${if (size > 1) s", ${children.map(_.toString).mkString(",")}"
        else ""})"
      else s"MutableTree(size=$size, width=$width, height=$height, hashCode=${hashCode()})"
    }

}

/** [[MutableTree]] factories. */
object MutableTree {

  /** Creates new empty [[MutableTree]] instance. */
  def apply[T](): MutableTree[T] =
    new MutableArrayTree(IntBuffer.empty, Buffer.empty[T])

  /** [[MutableTree]] implementation using buffers to store tree linearisation.  */
  final class MutableArrayTree[T] private[tree] (
    val structureBuffer: IntBuffer,
    val contentBuffer: Buffer[T]
  ) extends ArrayTreeLike[MutableTree, T] with MutableTree[T] {

    @`inline` override protected def structure: IntSlice = structureBuffer.asSlice
    @`inline` override protected def content: Slice[T] = contentBuffer.asSlice
    @`inline` override protected def tree: MutableTree[T] = this

    override def size: Int = structureBuffer.length
    override def width: Int = ArrayTreeFunctions.calculateWidth(structureBuffer)
    override def height: Int = ArrayTreeFunctions.calculateHeight(structureBuffer)
    override def isLeaf: Boolean = size == 1
    override def isEmpty: Boolean = size == 0
    override def childrenCount: Int = structureBuffer.head

    @`inline` override def immutable: Tree[T] =
      Transformer.OfTree.fromSlices(structureBuffer.asSlice.detach, contentBuffer.asSlice.detach)
  }

  implicit def asImmutable[T](tree: MutableTree[T]): Tree[T] =
    tree.immutable

  implicit def asMutable[T](tree: Tree[T]): MutableTree[T] =
    tree.mutable

  implicit def asMutable[T](trees: Iterable[Tree[T]]): Iterable[MutableTree[T]] =
    trees.map(_.mutable)

}
