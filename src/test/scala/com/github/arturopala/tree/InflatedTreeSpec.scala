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

import com.github.arturopala.tree.Tree.NodeTree
import com.github.arturopala.tree.TreeFormat.showAsGraph

class InflatedTreeSpec extends TreeSpec {

  override def name = "An inflated Tree"

  override val tree0: Tree[String] = TestTrees.tree0
  override val tree1: Tree[String] = TestTrees.tree1
  override val tree2: Tree[String] = TestTrees.tree2
  override val tree3_1: Tree[String] = TestTrees.tree3_1
  override val tree3_2: Tree[String] = TestTrees.tree3_2
  override val tree4_1: Tree[String] = TestTrees.tree4_1
  override val tree4_2: Tree[String] = TestTrees.tree4_2
  override val tree4_3: Tree[String] = TestTrees.tree4_3
  override val tree7: Tree[String] = TestTrees.tree7
  override val tree9: Tree[String] = TestTrees.tree9
  override val allTrees: Seq[Tree[String]] = TestTrees.allTrees

  "NodeTree" should {

    "mapUnsafe all nodes" in {
      val f: String => String = _ + "0"

      val result1 = tree1.asInstanceOf[NodeTree[String]].mapUnsafe(f)
      showAsGraph(result1, "\n") shouldBe "a0"

      val result2 = tree2.asInstanceOf[NodeTree[String]].mapUnsafe(f)
      showAsGraph(result2, "\n") shouldBe
        """a0 > b0""".stripMargin

      val result3_1 = tree3_1.asInstanceOf[NodeTree[String]].mapUnsafe(f)
      showAsGraph(result3_1, "\n") shouldBe
        """a0 > b0 > c0""".stripMargin

      val result3_2 = tree3_2.asInstanceOf[NodeTree[String]].mapUnsafe(f)
      showAsGraph(result3_2, "\n") shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin

      val result4 = tree7.asInstanceOf[NodeTree[String]].mapUnsafe(f)
      showAsGraph(result4, "\n") shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }
  }

}
