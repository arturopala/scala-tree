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

/** Interface of transformations to and from tree linearisation. */
trait Transformer[F[_]] {

  /** Outputs tree linearisation as a pair of slices. */
  def toSlices[T](target: F[T]): (IntSlice, Slice[T])

  /** Creates an instance from a pair of slices. */
  def fromSlices[T](structure: IntSlice, values: Slice[T]): F[T]

  /** Outputs tree linearisation as a pair of buffers. */
  def toBuffers[T, T1 >: T](target: F[T]): (IntBuffer, Buffer[T1])

  /** Creates an instance from a pair of buffers. */
  def fromBuffers[T](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): F[T]
}
