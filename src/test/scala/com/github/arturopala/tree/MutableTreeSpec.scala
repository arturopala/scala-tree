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

class MutableTreeSpec extends AnyWordSpecCompat with InflatedTestTrees {

  "MutableTree" should {

    "convert from and to the immutable tree" in {
      TestTrees.allTrees.foreach(tree => tree.mutable.immutable shouldBe tree)
    }

    "support operations on an empty tree" in {
      tree0.mutable.insertLeaf("a").immutable shouldBe tree1
      tree0.mutable.insertLeaves(List("a")).immutable shouldBe tree1
      tree0.mutable.insertLeaves(List("a", "b")).immutable shouldBe tree0
      tree0.mutable.insertChild(Tree("a")).immutable shouldBe tree1
      tree0.mutable.insertChildren(List(Tree("a"))).immutable shouldBe tree1
      tree0.mutable.insertBranch(List("a")).immutable shouldBe tree1
      tree0.mutable.insertBranch(List("a", "b")).immutable shouldBe tree2
      tree0.mutable.insertBranch(List("a", "b", "c")).immutable shouldBe tree3_1
    }
  }

}
