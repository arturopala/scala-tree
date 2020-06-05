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
import com.github.arturopala.tree.LaxTreeOps._
import com.github.arturopala.tree.internal.ArrayTree._
import com.github.arturopala.tree.internal.ArrayTreeFunctions
import com.github.arturopala.tree.internal.ArrayTreeFunctions.{expandValueIntoTreeDistinct, insertBeforeChildren, insertBeforeChildrenDistinct, insertBetweenChildrenDistinct, makeChildrenDistinct, mergeShallowTwoTrees}

import scala.reflect.ClassTag

// Special test suite to ease debugging single assertions in an IDE
class TreeDebugSpec extends FunSuite with TestWithBuffers {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "debug" in {

      tree2.insertLeafLaxAt(List("a", "c"), "a", append = true) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))

      /*testWithBuffers[String, Int](
        (structureBuffer: IntBuffer, valuesBuffer: Buffer[String]) =>
          expandValueIntoTreeDistinct(0, 2, IntSlice(0), Slice("b"), structureBuffer, valuesBuffer),
        IntBuffer(0, 0, 2),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -1
      }

      tree2.insertChildren(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("c"), Tree("b"))

      tree7.updateChild("b", Tree("d", Tree("e", Tree("g")))) shouldBe
        Tree("a", Tree("d", Tree("e", Tree("g"), Tree("f"))), Tree("g"))

      testWithBuffers[String, Int](
        insertBetweenChildrenDistinct(3, IntSlice(0, 1), Slice("e", "b"), false, _, _),
        IntBuffer(0, 1, 0, 1, 0, 0, 2, 3),
        Buffer("d", "b", "d", "c", "d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 0, 1, 0, 0, 0, 3, 3)
          values shouldBe Array("d", "b", "d", "c", "e", "d", "c", "b", "a")
          delta shouldBe 1
      }

      testWithBuffers[String, Int](
        insertBetweenChildrenDistinct(4, IntSlice(0, 1, 1), Slice("g", "e", "d"), true, _, _),
        IntBuffer(0, 0, 1, 1, 0, 3),
        Buffer("g", "f", "e", "d", "b", "a")
      ) {
        case (structure, values, delta) =>
          values shouldBe Array("g", "g", "f", "e", "d", "b", "a")
          structure shouldBe Array(0, 0, 0, 2, 1, 0, 3)
          delta shouldBe 1
      }

      testWithBuffers[String, Int](
        insertBetweenChildrenDistinct(4, IntSlice(0, 1, 1), Slice("g", "e", "d"), false, _, _),
        IntBuffer(0, 0, 1, 1, 0, 3),
        Buffer("g", "f", "e", "d", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 2, 1, 0, 3)
          values shouldBe Array("g", "f", "g", "e", "d", "b", "a")
          delta shouldBe 1
      }

      testWithBuffers[String, Int](
        insertBeforeChildrenDistinct(3, IntSlice(0, 0, 2), Slice("e", "c", "b"), _, _),
        IntBuffer(0, 0, 2, 1),
        Buffer("d", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3, 1)
          values shouldBe Array("d", "c", "e", "b", "a")
          delta shouldBe 1
      }

      testWithBuffers[String, (Int, Int)](
        mergeShallowTwoTrees(3, 6, _, _),
        IntBuffer(0, 0, 1, 2, 0, 1, 1, 2),
        Buffer("h", "g", "f", "e", "d", "c", "b", "a")
      ) {
        case (structure, values, (delta, index)) =>
          structure shouldBe Array(0, 0, 1, 0, 1, 3, 1)
          values shouldBe Array("h", "g", "f", "d", "c", "e", "a")
          delta shouldBe -1
          index shouldBe 5
      }

      testWithBuffers[String, Int](
        insertBeforeChildrenDistinct(2, IntSlice(0, 1), Slice("d", "b"), _, _),
        IntBuffer(0, 1, 1),
        Buffer("c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 2, 1)
          values shouldBe Array("c", "d", "b", "a")
          delta shouldBe 1
      }

      testWithBuffers[String, Int](
        insertBeforeChildrenDistinct(-1, IntSlice(0), Slice("a"), _, _),
        IntBuffer(0),
        Buffer("a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 0
      }

      testWithBuffers[String, Int](
        makeChildrenDistinct(4, true, _, _),
        IntBuffer(0, 1, 0, 1, 2),
        Buffer("c", "b", "c", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1, 1)
          values shouldBe Array("c", "b", "a")
          delta shouldBe -2
      }

      testWithBuffers[String, Int](
        makeChildrenDistinct(3, true, _, _),
        IntBuffer(0, 0, 0, 3),
        Buffer("b", "b", "b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 1)
          values shouldBe Array("b", "a")
          delta shouldBe -2
      }

      tree1.insertChildren(List(Tree("b", Tree("c", Tree("d")), Tree("f")), Tree("b", Tree("c", Tree("e")), Tree("g")))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e")), Tree("f"), Tree("g")))

      tree1.insertChildren(List(Tree.empty, Tree("a", Tree("c")).deflated, Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("a", Tree("c")), Tree("b"))

      tree1.insertChildren(List(Tree.empty, Tree("a"), Tree.empty, Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))

      testWithBuffers[String, Int](
        insertBeforeChildren(
          1,
          List((IntSlice(0), Slice("a")), (IntSlice(0), Slice("b")), (IntSlice(0), Slice("c"))),
          _,
          _,
          keepDistinct = true
        ),
        IntBuffer(0, 1),
        Buffer("b", "a")
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0, 0, 0, 3)
          values shouldBe Array("b", "c", "a", "a")
          delta shouldBe 2
      }

      testWithBuffers[String, Int](
        insertBeforeChildrenDistinct(-1, IntSlice(0), Slice("a"), _, _),
        IntBuffer.empty,
        Buffer.empty[String]
      ) {
        case (structure, values, delta) =>
          structure shouldBe Array(0)
          values shouldBe Array("a")
          delta shouldBe 1
      }*/
    }

  }

}
