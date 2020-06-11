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
import com.github.arturopala.tree.internal.{ArrayTreeFunctions, NodeTree}
import com.github.arturopala.tree.internal.ArrayTreeFunctions.{expandValueIntoTreeDistinct, insertBeforeChildDistinct, insertBeforeChildren, insertBetweenChildrenDistinct, makeChildrenDistinct, mergeShallowTwoTrees}

import scala.reflect.ClassTag

// Special test suite to ease debugging single assertions in an IDE
class TreeDebugSpec extends FunSuite with TestWithBuffers {

  //test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "debug" suite {

      test(
        tree(
          Tree(
            "a",
            Tree("b", Tree("c", Tree("d"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("d")))
          )
        ).insertChildren(List(Tree("b", Tree("c", Tree("e"))), Tree("b", Tree("c", Tree("f"))))) shouldBe
          Tree(
            "a",
            Tree("b", Tree("c", Tree("e"), Tree("f"), Tree("d"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("d")))
          )
      )

      test(
        tree1.insertChildren(List(Tree("a"), Tree("b"), Tree("a"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      )

      test(
        tree1.insertChildren(
          List(Tree("b", Tree("c", Tree("d")), Tree("f")), Tree("b", Tree("c", Tree("e")), Tree("g")))
        ) shouldBe
          Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e")), Tree("f"), Tree("g")))
      )

      test(
        tree(
          Tree(
            "a",
            Tree("b", Tree("c", Tree("d"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("d")))
          )
        ).insertChildren(List(Tree("b", Tree("c", Tree("e"))), Tree("b", Tree("c", Tree("f")))), append = true) shouldBe
          Tree(
            "a",
            Tree("b", Tree("c", Tree("d"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("e"))),
            Tree("b", Tree("c", Tree("d"), Tree("e"), Tree("f")))
          )
      )
    }

  }

}
