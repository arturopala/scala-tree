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

import scala.reflect.ClassTag

class TreeCheckSelectSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "check if the tree contains a branch" in {
      tree0.containsBranch(List()) shouldBe false
      tree0.containsBranch(List("a")) shouldBe false
      tree0.containsBranch(List("a", "b")) shouldBe false
      tree1.containsBranch(List("a")) shouldBe true
      tree1.containsBranch(List("b")) shouldBe false
      tree1.containsBranch(List("a", "b")) shouldBe false
      tree2.containsBranch(List("a")) shouldBe false
      tree2.containsBranch(List("a", "b")) shouldBe true
      tree2.containsBranch(List("a", "c")) shouldBe false
      tree2.containsBranch(List("a", "b", "c")) shouldBe false
      tree3_1.containsBranch(List("a", "b", "c")) shouldBe true
      tree3_1.containsBranch(List("a", "b")) shouldBe false
      tree3_1.containsBranch(List("a", "c")) shouldBe false
      tree3_1.containsBranch(List("a")) shouldBe false
      tree3_1.containsBranch(List("b")) shouldBe false
      tree3_1.containsBranch(List("b", "c")) shouldBe false
      tree3_2.containsBranch(List("a", "b")) shouldBe true
      tree3_2.containsBranch(List("a", "c")) shouldBe true
      tree3_2.containsBranch(List("a", "a")) shouldBe false
      tree3_2.containsBranch(List("a")) shouldBe false
      tree3_2.containsBranch(List("b")) shouldBe false
      tree3_2.containsBranch(List("c")) shouldBe false
      tree3_2.containsBranch(List("a", "b", "c")) shouldBe false
      tree3_2.containsBranch(List("a", "c", "b")) shouldBe false
      tree3_2.containsBranch(List("a", "c", "e")) shouldBe false
      tree4_1.containsBranch(List("a", "b", "c", "d")) shouldBe true
      tree4_1.containsBranch(List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.containsBranch(List("a", "b", "c")) shouldBe false
      tree4_1.containsBranch(List("a", "b")) shouldBe false
      tree4_1.containsBranch(List("a")) shouldBe false
      tree4_2.containsBranch(List("a", "b", "c")) shouldBe true
      tree4_2.containsBranch(List("a", "d")) shouldBe true
      tree4_2.containsBranch(List("a", "b", "c", "d")) shouldBe false
      tree4_2.containsBranch(List("a", "a", "a")) shouldBe false
      tree4_2.containsBranch(List("c", "b", "a")) shouldBe false
      tree4_2.containsBranch(List("d", "a")) shouldBe false
      tree9.containsBranch(List("a", "b", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "h", "i")) shouldBe true
      tree9.containsBranch(List("a", "e", "c")) shouldBe false
      tree9.containsBranch(List("a", "e", "f")) shouldBe false
      tree9.containsBranch(List("a", "e", "h")) shouldBe false
    }

    "check if the tree contains a branch using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.containsBranch(List(), codeF) shouldBe false
      tree0.containsBranch(List(97), codeF) shouldBe false
      tree0.containsBranch(List(97, 98), codeF) shouldBe false
      tree1.containsBranch(List(97), codeF) shouldBe true
      tree1.containsBranch(List(98), codeF) shouldBe false
      tree1.containsBranch(List(97, 98), codeF) shouldBe false
      tree2.containsBranch(List(97), codeF) shouldBe false
      tree2.containsBranch(List(97, 98), codeF) shouldBe true
      tree2.containsBranch(List(97, 99), codeF) shouldBe false
      tree2.containsBranch(List(97, 98, 99), codeF) shouldBe false
      tree3_1.containsBranch(List(97, 98, 99), codeF) shouldBe true
      tree3_1.containsBranch(List(97, 98), codeF) shouldBe false
      tree3_1.containsBranch(List(97, 99), codeF) shouldBe false
      tree3_1.containsBranch(List(97), codeF) shouldBe false
      tree3_1.containsBranch(List(98), codeF) shouldBe false
      tree3_1.containsBranch(List(98, 99), codeF) shouldBe false
      tree3_2.containsBranch(List(97, 98), codeF) shouldBe true
      tree3_2.containsBranch(List(97, 99), codeF) shouldBe true
      tree3_2.containsBranch(List(97, 97), codeF) shouldBe false
      tree3_2.containsBranch(List(97), codeF) shouldBe false
      tree3_2.containsBranch(List(98), codeF) shouldBe false
      tree3_2.containsBranch(List(99), codeF) shouldBe false
      tree3_2.containsBranch(List(97, 98, 99), codeF) shouldBe false
      tree3_2.containsBranch(List(97, 99, 98), codeF) shouldBe false
      tree3_2.containsBranch(List(97, 99, 101), codeF) shouldBe false
      tree4_1.containsBranch(List(97, 98, 99, 100), codeF) shouldBe true
      tree4_1.containsBranch(List(97, 98, 99, 100, 101), codeF) shouldBe false
      tree4_1.containsBranch(List(97, 98, 99), codeF) shouldBe false
      tree4_1.containsBranch(List(97, 98), codeF) shouldBe false
      tree4_1.containsBranch(List(97), codeF) shouldBe false
      tree4_2.containsBranch(List(97, 98, 99), codeF) shouldBe true
      tree4_2.containsBranch(List(97, 100), codeF) shouldBe true
      tree4_2.containsBranch(List(97, 98, 99, 100), codeF) shouldBe false
      tree4_2.containsBranch(List(97, 97, 97), codeF) shouldBe false
      tree4_2.containsBranch(List(99, 98, 97), codeF) shouldBe false
      tree4_2.containsBranch(List(100, 97), codeF) shouldBe false
      tree9.containsBranch(List(97, 98, 99), codeF) shouldBe false
      tree9.containsBranch(List(97, 101, 104, 105), codeF) shouldBe true
      tree9.containsBranch(List(97, 101, 99), codeF) shouldBe false
      tree9.containsBranch(List(97, 101, 102), codeF) shouldBe false
      tree9.containsBranch(List(97, 101, 104), codeF) shouldBe false
    }

    "check if the tree contains a path" in {
      tree0.containsPath(List()) shouldBe false
      tree0.containsPath(List("a")) shouldBe false
      tree0.containsPath(List("a", "b")) shouldBe false
      tree1.containsPath(List("a")) shouldBe true
      tree1.containsPath(List("b")) shouldBe false
      tree1.containsPath(List("a", "b")) shouldBe false
      tree2.containsPath(List("a")) shouldBe true
      tree2.containsPath(List("a", "b")) shouldBe true
      tree2.containsPath(List("a", "c")) shouldBe false
      tree2.containsPath(List("a", "b", "c")) shouldBe false
      tree3_1.containsPath(List("a", "b", "c")) shouldBe true
      tree3_1.containsPath(List("a", "b")) shouldBe true
      tree3_1.containsPath(List("a", "c")) shouldBe false
      tree3_1.containsPath(List("a")) shouldBe true
      tree3_1.containsPath(List("b")) shouldBe false
      tree3_1.containsPath(List("b", "c")) shouldBe false
      tree3_2.containsPath(List("a", "b")) shouldBe true
      tree3_2.containsPath(List("a", "c")) shouldBe true
      tree3_2.containsPath(List("a", "a")) shouldBe false
      tree3_2.containsPath(List("a")) shouldBe true
      tree3_2.containsPath(List("b")) shouldBe false
      tree3_2.containsPath(List("c")) shouldBe false
      tree3_2.containsPath(List("a", "b", "c")) shouldBe false
      tree3_2.containsPath(List("a", "c", "b")) shouldBe false
      tree3_2.containsPath(List("a", "c", "e")) shouldBe false
      tree4_1.containsPath(List("a", "b", "c", "d")) shouldBe true
      tree4_1.containsPath(List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.containsPath(List("a", "b", "c")) shouldBe true
      tree4_1.containsPath(List("a", "b")) shouldBe true
      tree4_1.containsPath(List("a")) shouldBe true
      tree4_2.containsPath(List("a", "b", "c")) shouldBe true
      tree4_2.containsPath(List("a", "d")) shouldBe true
      tree4_2.containsPath(List("a", "b", "c", "d")) shouldBe false
      tree4_2.containsPath(List("a", "b")) shouldBe true
      tree4_2.containsPath(List("a", "a", "a")) shouldBe false
      tree4_2.containsPath(List("c", "b", "a")) shouldBe false
      tree4_2.containsPath(List("d", "a")) shouldBe false
      tree9.containsPath(List("a", "b", "c")) shouldBe true
      tree9.containsPath(List("a", "e", "h", "i")) shouldBe true
      tree9.containsPath(List("a", "e", "f")) shouldBe true
      tree9.containsPath(List("a", "e", "h")) shouldBe true
    }

    "check if the tree contains a path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.containsPath(List(), codeF) shouldBe false
      tree0.containsPath(List(97), codeF) shouldBe false
      tree0.containsPath(List(97, 98), codeF) shouldBe false
      tree1.containsPath(List(97), codeF) shouldBe true
      tree1.containsPath(List(98), codeF) shouldBe false
      tree1.containsPath(List(97, 98), codeF) shouldBe false
      tree2.containsPath(List(97), codeF) shouldBe true
      tree2.containsPath(List(97, 98), codeF) shouldBe true
      tree2.containsPath(List(97, 99), codeF) shouldBe false
      tree2.containsPath(List(97, 98, 99), codeF) shouldBe false
      tree3_1.containsPath(List(97, 98, 99), codeF) shouldBe true
      tree3_1.containsPath(List(97, 98), codeF) shouldBe true
      tree3_1.containsPath(List(97, 99), codeF) shouldBe false
      tree3_1.containsPath(List(97), codeF) shouldBe true
      tree3_1.containsPath(List(98), codeF) shouldBe false
      tree3_1.containsPath(List(98, 99), codeF) shouldBe false
      tree3_2.containsPath(List(97, 98), codeF) shouldBe true
      tree3_2.containsPath(List(97, 99), codeF) shouldBe true
      tree3_2.containsPath(List(97, 97), codeF) shouldBe false
      tree3_2.containsPath(List(97), codeF) shouldBe true
      tree3_2.containsPath(List(98), codeF) shouldBe false
      tree3_2.containsPath(List(99), codeF) shouldBe false
      tree3_2.containsPath(List(97, 98, 99), codeF) shouldBe false
      tree3_2.containsPath(List(97, 99, 98), codeF) shouldBe false
      tree3_2.containsPath(List(97, 99, 101), codeF) shouldBe false
      tree4_1.containsPath(List(97, 98, 99, 100), codeF) shouldBe true
      tree4_1.containsPath(List(97, 98, 99, 100, 101), codeF) shouldBe false
      tree4_1.containsPath(List(97, 98, 99), codeF) shouldBe true
      tree4_1.containsPath(List(97, 98), codeF) shouldBe true
      tree4_1.containsPath(List(97), codeF) shouldBe true
      tree4_2.containsPath(List(97, 98, 99), codeF) shouldBe true
      tree4_2.containsPath(List(97, 100), codeF) shouldBe true
      tree4_2.containsPath(List(97, 98, 99, 100), codeF) shouldBe false
      tree4_2.containsPath(List(97, 98), codeF) shouldBe true
      tree4_2.containsPath(List(97, 97, 97), codeF) shouldBe false
      tree4_2.containsPath(List(99, 98, 97), codeF) shouldBe false
      tree4_2.containsPath(List(100, 97), codeF) shouldBe false
      tree9.containsPath(List(97, 98, 99), codeF) shouldBe true
      tree9.containsPath(List(97, 101, 104, 105), codeF) shouldBe true
      tree9.containsPath(List(97, 101, 102), codeF) shouldBe true
      tree9.containsPath(List(97, 101, 104), codeF) shouldBe true
    }

    "select a value by a path" in {
      tree0.selectValue(List(), _.length) shouldBe None
      tree1.selectValue(List(), _.length) shouldBe None
      tree1.selectValue(List(1), _.length) shouldBe Some("a")
      tree1.selectValue(List("a"), identity) shouldBe Some("a")
      tree1.selectValue(List("a", "b"), identity) shouldBe None
      tree2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree2.selectValue(List(1, 1), _.length) shouldBe Some("b")
      tree2.selectValue(List(1), _.length) shouldBe Some("a")
      tree2.selectValue(List(1, 0), _.length) shouldBe None
      tree2.selectValue(List(0, 1), _.length) shouldBe None
      tree2.selectValue(List(0, 0), _.length) shouldBe None
      tree3_1.selectValue(List(), _.length) shouldBe None
      tree3_1.selectValue(List(1, 1, 1), _.length) shouldBe Some("c")
      tree3_1.selectValue(List(1, 1), _.length) shouldBe Some("b")
      tree3_1.selectValue(List(1), _.length) shouldBe Some("a")
      tree3_2.selectValue(List(), _.length) shouldBe None
      tree3_2.selectValue(List(1), _.length) shouldBe Some("a")
      tree3_2.selectValue(List(1, 1), _.length) shouldBe Some("b")
      tree3_2.selectValue(List("a"), identity) shouldBe Some("a")
      tree3_2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree3_2.selectValue(List("a", "c"), identity) shouldBe Some("c")
      tree3_2.selectValue(List("a", "d"), identity) shouldBe None
      tree3_2.selectValue(List("a", "a"), identity) shouldBe None
      tree3_2.selectValue(List("c", "a"), identity) shouldBe None
      tree3_2.selectValue(List("b", "a"), identity) shouldBe None
      tree4_1.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_1.selectValue(List("a", "b", "c"), identity) shouldBe Some("c")
      tree4_1.selectValue(List("a", "b", "c", "d"), identity) shouldBe Some("d")
      tree4_1.selectValue(List("a", "c"), identity) shouldBe None
      tree4_1.selectValue(List("a", "d"), identity) shouldBe None
      tree4_2.selectValue(List(), identity) shouldBe None
      tree4_2.selectValue(List("a"), identity) shouldBe Some("a")
      tree4_2.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_2.selectValue(List("a", "b", "d"), identity) shouldBe None
      tree4_2.selectValue(List("a", "b", "c"), identity) shouldBe Some("c")
      tree4_2.selectValue(List("a", "d"), identity) shouldBe Some("d")
      tree4_2.selectValue(List("a", "d", "c"), identity) shouldBe None
      tree4_2.selectValue(List("b", "c"), identity) shouldBe None
      tree4_2.selectValue(List("d"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b"), identity) shouldBe Some("b")
      tree4_3.selectValue(List(), identity) shouldBe None
      tree4_3.selectValue(List("a"), identity) shouldBe Some("a")
      tree4_3.selectValue(List("a", "c"), identity) shouldBe Some("c")
      tree4_3.selectValue(List("a", "d"), identity) shouldBe Some("d")
      tree4_3.selectValue(List("a", "a"), identity) shouldBe None
      tree4_3.selectValue(List("a", "e"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b", "c"), identity) shouldBe None
      tree4_3.selectValue(List("a", "b", "b"), identity) shouldBe None
      tree4_3.selectValue(List("a", "a", "a"), identity) shouldBe None
      tree9.selectValue(List("a", "b", "c", "d"), identity) shouldBe Some("d")
      tree9.selectValue(List("a", "e", "f", "g"), identity) shouldBe Some("g")
      tree9.selectValue(List("a", "e", "h", "i"), identity) shouldBe Some("i")
      tree9.selectValue(List("a", "e", "f", "i"), identity) shouldBe None
      // test leftmost and rightmost selection
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectValue(List(1, 1, 1), _.length) shouldBe Some("c")
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectValue(List(1, 1, 1), _.length, rightmost = true) shouldBe Some("e")
    }

    "select a tree by a path" in {
      tree0.selectTree(List()) shouldBe None
      tree0.selectTree(List("a")) shouldBe None
      tree1.selectTree(List("a")) shouldBe Some(Tree("a"))
      tree1.selectTree(List("a", "b")) shouldBe None
      tree2.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b"), Tree("c")))
      tree3_2.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List("a", "c")) shouldBe Some(Tree("c"))
      tree3_2.selectTree(List("a", "d")) shouldBe None
      tree3_2.selectTree(List("a", "a")) shouldBe None
      tree3_2.selectTree(List("c", "a")) shouldBe None
      tree3_2.selectTree(List("b", "a")) shouldBe None
      tree4_1.selectTree(List("a", "b")) shouldBe Some(Tree("b", Tree("c", Tree("d"))))
      tree4_1.selectTree(List("a", "b", "c")) shouldBe Some(Tree("c", Tree("d")))
      tree4_1.selectTree(List("a", "b", "c", "d")) shouldBe Some(Tree("d"))
      tree4_1.selectTree(List("a", "c")) shouldBe None
      tree4_1.selectTree(List("a", "d")) shouldBe None
      tree4_2.selectTree(List()) shouldBe None
      tree4_2.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b", Tree("c")), Tree("d")))
      tree4_2.selectTree(List("a", "b")) shouldBe Some(Tree("b", Tree("c")))
      tree4_2.selectTree(List("a", "b", "d")) shouldBe None
      tree4_2.selectTree(List("a", "b", "c")) shouldBe Some(Tree("c"))
      tree4_2.selectTree(List("a", "d")) shouldBe Some(Tree("d"))
      tree4_2.selectTree(List("a", "d", "c")) shouldBe None
      tree4_2.selectTree(List("b", "c")) shouldBe None
      tree4_2.selectTree(List("d")) shouldBe None
      tree4_3.selectTree(List("a", "b")) shouldBe Some(Tree("b"))
      tree4_3.selectTree(List()) shouldBe None
      tree4_3.selectTree(List("a")) shouldBe Some(Tree("a", Tree("b"), Tree("c"), Tree("d")))
      tree4_3.selectTree(List("a", "c")) shouldBe Some(Tree("c"))
      tree4_3.selectTree(List("a", "d")) shouldBe Some(Tree("d"))
      tree4_3.selectTree(List("a", "a")) shouldBe None
      tree4_3.selectTree(List("a", "e")) shouldBe None
      tree4_3.selectTree(List("a", "b", "c")) shouldBe None
      tree4_3.selectTree(List("a", "b", "b")) shouldBe None
      tree4_3.selectTree(List("a", "a", "a")) shouldBe None
      tree9.selectTree(List("a", "b", "c", "d")) shouldBe Some(Tree("d"))
      tree9.selectTree(List("a", "e", "f", "g")) shouldBe Some(Tree("g"))
      tree9.selectTree(List("a", "e", "h", "i")) shouldBe Some(Tree("i"))
      tree9.selectTree(List("a", "e", "f", "i")) shouldBe None

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List("a", "b")) shouldBe
        Some(Tree("b", Tree("c")))
    }

    "select a tree by path using extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.selectTree(List(), codeF) shouldBe None
      tree0.selectTree(List(0), codeF) shouldBe None
      tree1.selectTree(List(), codeF) shouldBe None
      tree1.selectTree(List(97), codeF) shouldBe Some(Tree("a"))
      tree1.selectTree(List(96), codeF) shouldBe None
      tree2.selectTree(List(97, 98), codeF) shouldBe Some(Tree("b"))
      tree2.selectTree(List(97, 97), codeF) shouldBe None
      tree2.selectTree(List(98, 97), codeF) shouldBe None
      tree3_2.selectTree(List(97, 98), codeF) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List(97, 99), codeF) shouldBe Some(Tree("c"))
      tree3_2.selectTree(List(97), codeF) shouldBe Some(Tree("a", Tree("b"), Tree("c")))
      tree3_2.selectTree(List(97, 97), codeF) shouldBe None
      tree3_2.selectTree(List(98), codeF) shouldBe None
      tree3_2.selectTree(List(), codeF) shouldBe None

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(97, 98), codeF) shouldBe
        Some(Tree("b", Tree("c")))
    }

  }

}
