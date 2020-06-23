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

import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.MutableTree.MutableArrayTree
import com.github.arturopala.tree.{MutableTree, Tree}
import com.github.arturopala.tree.Tree.ArrayTree

/** Transformation to and from tree linearisation typeclass. */
trait Transformer[F[+_]] {

  /** Outputs tree linearisation as a pair of slices. */
  def toSlices[T](target: F[T]): (IntSlice, Slice[T])

  /** Creates an instance from a pair of slices. */
  def fromSlices[T](structure: IntSlice, values: Slice[T]): F[T]

  /** Outputs tree linearisation as a pair of buffers. */
  def toBuffers[T, T1 >: T](target: F[T]): (IntBuffer, Buffer[T1])

  /** Creates an instance from a pair of buffers. */
  def fromBuffers[T](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): F[T]

  /** Returns size of a structure of an instance. */
  def sizeOf[T](target: F[T]): Int

  /** Returns true if an instance is empty. */
  def isEmpty[T](target: F[T]): Boolean

  /** Returns an empty instance. */
  def empty[T]: F[T]
}

object Transformer {

  /** Transformer instance for the Tree. */
  implicit object OfTree extends Transformer[Tree] {

    override def toSlices[T](target: Tree[T]): (IntSlice, Slice[T]) =
      target.toSlices

    /** Creates a tree from a pair of slices. */
    override def fromSlices[T](structure: IntSlice, values: Slice[T]): Tree[T] =
      if (structure.length == 0) Tree.empty
      else
        new ArrayTree[T](
          structure,
          values,
          ArrayTreeFunctions.calculateWidth(structure),
          ArrayTreeFunctions.calculateHeight(structure)
        )

    /** Outputs tree linearisation as a pair of buffers. */
    override def toBuffers[T, T1 >: T](target: Tree[T]): (IntBuffer, Buffer[T1]) =
      target.toBuffers

    /** Creates a tree from a pair of buffers. */
    override def fromBuffers[T](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Tree[T] =
      fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)

    /** Returns size of a structure of an instance. */
    override def sizeOf[T](target: Tree[T]): Int = target.size

    /** Returns true if an instance is empty. */
    override def isEmpty[T](target: Tree[T]): Boolean = target.isEmpty

    /** Returns an empty instance. */
    override def empty[T]: Tree[T] = Tree.empty
  }

  /** Transformer instance for the MutableTree. */
  implicit object OfMutableTree extends Transformer[MutableTree] {

    override def toSlices[T](target: MutableTree[T]): (IntSlice, Slice[T]) =
      target.toSlices

    /** Creates a tree from a pair of slices. */
    override def fromSlices[T](structure: IntSlice, values: Slice[T]): MutableTree[T] =
      fromBuffers(structure.asBuffer, values.asBuffer)

    /** Outputs tree linearisation as a pair of buffers. */
    override def toBuffers[T, T1 >: T](target: MutableTree[T]): (IntBuffer, Buffer[T1]) =
      target.toBuffers

    /** Creates a tree from a pair of buffers. */
    override def fromBuffers[T](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): MutableTree[T] =
      new MutableArrayTree[T](structureBuffer, valuesBuffer)

    /** Returns size of a structure of an instance. */
    override def sizeOf[T](target: MutableTree[T]): Int = target.size

    /** Returns true if an instance is empty. */
    override def isEmpty[T](target: MutableTree[T]): Boolean = target.isEmpty

    /** Returns an empty instance. */
    override def empty[T]: MutableTree[T] = MutableTree()
  }

}
