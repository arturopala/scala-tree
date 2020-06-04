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
import com.github.arturopala.tree.internal.ArrayTreeFunctions.insertLeftChildren

import scala.reflect.ClassTag

// Special test suite to ease single assertions debugging in an IDE
class TreeDebugSpec extends FunSuite with TestWithBuffers {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "debug" in {

      tree2.insertChildren(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("c"), Tree("b"))

      tree1.insertChildren(List(Tree("b", Tree("c", Tree("d")), Tree("f")), Tree("b", Tree("c", Tree("e")), Tree("g")))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e")), Tree("f"), Tree("g")))

      tree1.insertChildren(List(Tree.empty, Tree("a", Tree("c")).deflated, Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("a", Tree("c")), Tree("b"))

      tree1.insertChildren(List(Tree.empty, Tree("a"), Tree.empty, Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))

      testWithBuffers[String, Int](
        insertLeftChildren(
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
    }

  }

}
