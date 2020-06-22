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
import com.github.arturopala.tree.internal.{ArrayTreeFunctions, NodeTree, Transformer}
import com.github.arturopala.tree.internal.ArrayTreeFunctions.{expandValueIntoTreeDistinct, insertBeforeChildDistinct, insertBeforeChildren, insertBetweenChildrenDistinct, makeChildrenDistinct, mergeShallowTwoTrees}

import scala.reflect.ClassTag

// Special purpose test suite to facilitate debugging single assertions in an IDE
class TreeDebugSpec extends FunSuite with TestWithBuffers {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "debug" suite {
      val f4: String => Tree[String] = s => Tree("b", Tree(s))
      test(
        tree3_2.flatMap(f4) shouldBe Tree("b", Tree("b", Tree("b"), Tree("c")), Tree("a"))
      )
    }

  }

}
