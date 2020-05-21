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

import scala.reflect.ClassTag

trait TestTrees {

  val tree0: Tree[String]
  val tree1: Tree[String]
  val tree2: Tree[String]
  val tree3_1: Tree[String]
  val tree3_2: Tree[String]
  val tree4_1: Tree[String]
  val tree4_2: Tree[String]
  val tree4_3: Tree[String]
  val tree7: Tree[String]
  val tree9: Tree[String]
  val allTrees: Seq[Tree[String]]
}

/** Collection of simple trees for use in testing. */
object TestTrees {

  val tree0: Tree[String] = Tree[String]()

  /**
    * [a]
    */
  val tree1: NodeTree[String] = Tree("a")

  /**
    * [a,b]
    */
  val tree2: NodeTree[String] = Tree("a", Tree("b"))

  /**
    * [a,b,c]
    */
  val tree3_1: NodeTree[String] = Tree("a", Tree("b", Tree("c")))

  /**
    * [a,b]
    * [a,c]
    */
  val tree3_2: NodeTree[String] = Tree("a", Tree("b"), Tree("c"))

  /**
    * [a,b,c,d]
    */
  val tree4_1: NodeTree[String] = Tree("a", Tree("b", Tree("c", Tree("d"))))

  /**
    * [a,b,c]
    * [a,d]
    */
  val tree4_2: NodeTree[String] = Tree("a", Tree("b", Tree("c")), Tree("d"))

  /**
    * [a,b]
    * [a,c]
    * [a,d]
    */
  val tree4_3: NodeTree[String] = Tree("a", Tree("b"), Tree("c"), Tree("d"))

  /**
    * [a,b,c]
    * [a,d,e,f]
    * [a,g]
    */
  val tree7: NodeTree[String] = Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))

  /**
    * [a,b,c,d]
    * [a,e,f,g]
    * [a,e,h,i]
    */
  val tree9: NodeTree[String] =
    Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i"))))

  val allTrees: Seq[Tree[String]] =
    Seq(tree0, tree1, tree2, tree3_1, tree3_2, tree4_1, tree4_2, tree4_3, tree7, tree9)

}

trait InflatedTestTrees extends TestTrees {

  final override val tree0: Tree[String] = TestTrees.tree0
  final override val tree1: Tree[String] = TestTrees.tree1
  final override val tree2: Tree[String] = TestTrees.tree2
  final override val tree3_1: Tree[String] = TestTrees.tree3_1
  final override val tree3_2: Tree[String] = TestTrees.tree3_2
  final override val tree4_1: Tree[String] = TestTrees.tree4_1
  final override val tree4_2: Tree[String] = TestTrees.tree4_2
  final override val tree4_3: Tree[String] = TestTrees.tree4_3
  final override val tree7: Tree[String] = TestTrees.tree7
  final override val tree9: Tree[String] = TestTrees.tree9
  final override val allTrees: Seq[Tree[String]] = TestTrees.allTrees

  final def tree[T: ClassTag](t: Tree[T]): Tree[T] = t.inflated

}

trait DeflatedTestTrees extends TestTrees {

  final override val tree0: Tree[String] = TestTrees.tree0.deflated
  final override val tree1: Tree[String] = TestTrees.tree1.deflated
  final override val tree2: Tree[String] = TestTrees.tree2.deflated
  final override val tree3_1: Tree[String] = TestTrees.tree3_1.deflated
  final override val tree3_2: Tree[String] = TestTrees.tree3_2.deflated
  final override val tree4_1: Tree[String] = TestTrees.tree4_1.deflated
  final override val tree4_2: Tree[String] = TestTrees.tree4_2.deflated
  final override val tree4_3: Tree[String] = TestTrees.tree4_3.deflated
  final override val tree7: Tree[String] = TestTrees.tree7.deflated
  final override val tree9: Tree[String] = TestTrees.tree9.deflated
  final override val allTrees: Seq[Tree[String]] = TestTrees.allTrees.map(_.deflated)

  final def tree[T: ClassTag](t: Tree[T]): Tree[T] = t.deflated
}
