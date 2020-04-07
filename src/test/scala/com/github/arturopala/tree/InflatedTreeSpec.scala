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

}
