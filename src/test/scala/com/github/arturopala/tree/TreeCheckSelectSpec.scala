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

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List("a", "b"), rightmost = true) shouldBe
        Some(Tree("b", Tree("e")))
    }

    "select a tree by path using extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.selectTree(List(), codeF, rightmost = false) shouldBe None
      tree0.selectTree(List(0), codeF, rightmost = false) shouldBe None
      tree1.selectTree(List(), codeF, rightmost = false) shouldBe None
      tree1.selectTree(List(97), codeF, rightmost = false) shouldBe Some(Tree("a"))
      tree1.selectTree(List(96), codeF, rightmost = false) shouldBe None
      tree2.selectTree(List(97, 98), codeF, rightmost = false) shouldBe Some(Tree("b"))
      tree2.selectTree(List(97, 97), codeF, rightmost = false) shouldBe None
      tree2.selectTree(List(98, 97), codeF, rightmost = false) shouldBe None
      tree3_2.selectTree(List(97, 98), codeF, rightmost = false) shouldBe Some(Tree("b"))
      tree3_2.selectTree(List(97, 99), codeF, rightmost = false) shouldBe Some(Tree("c"))
      tree3_2.selectTree(List(97), codeF, rightmost = false) shouldBe Some(Tree("a", Tree("b"), Tree("c")))
      tree3_2.selectTree(List(97, 97), codeF, rightmost = false) shouldBe None
      tree3_2.selectTree(List(98), codeF, rightmost = false) shouldBe None
      tree3_2.selectTree(List(), codeF, rightmost = false) shouldBe None

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(97, 98), codeF, rightmost = false) shouldBe
        Some(Tree("b", Tree("c")))

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(1, 1), _.length, rightmost = false) shouldBe
        Some(Tree("b", Tree("c")))

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(1, 1, 1), _.length, rightmost = false) shouldBe
        Some(Tree("c"))

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(97, 98), codeF, rightmost = true) shouldBe
        Some(Tree("b", Tree("e")))

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(1, 1), _.length, rightmost = true) shouldBe
        Some(Tree("b", Tree("e")))

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .selectTree(List(1, 1, 1), _.length, rightmost = true) shouldBe
        Some(Tree("e"))
    }

    "check if the tree contains value" in {
      tree0.containsValue("a") shouldBe false
      tree1.containsValue("a") shouldBe true
      tree1.containsValue("b") shouldBe false
      tree2.containsValue("a") shouldBe true
      tree2.containsValue("b") shouldBe true
      tree3_1.containsValue("a") shouldBe true
      tree3_1.containsValue("b") shouldBe true
      tree3_1.containsValue("c") shouldBe true
      tree3_1.containsValue("d") shouldBe false
      tree3_2.containsValue("a") shouldBe true
      tree3_2.containsValue("b") shouldBe true
      tree3_2.containsValue("c") shouldBe true
      tree3_2.containsValue("d") shouldBe false
      tree4_1.containsValue("a") shouldBe true
      tree4_1.containsValue("b") shouldBe true
      tree4_1.containsValue("c") shouldBe true
      tree4_1.containsValue("d") shouldBe true
      tree4_1.containsValue("e") shouldBe false
      tree4_2.containsValue("a") shouldBe true
      tree4_2.containsValue("b") shouldBe true
      tree4_2.containsValue("c") shouldBe true
      tree4_2.containsValue("d") shouldBe true
      tree4_2.containsValue("e") shouldBe false
      tree4_3.containsValue("a") shouldBe true
      tree4_3.containsValue("b") shouldBe true
      tree4_3.containsValue("c") shouldBe true
      tree4_3.containsValue("d") shouldBe true
      tree4_3.containsValue("e") shouldBe false
      tree7.containsValue("a") shouldBe true
      tree7.containsValue("b") shouldBe true
      tree7.containsValue("c") shouldBe true
      tree7.containsValue("d") shouldBe true
      tree7.containsValue("e") shouldBe true
      tree7.containsValue("f") shouldBe true
      tree7.containsValue("g") shouldBe true
      tree7.containsValue("h") shouldBe false
    }

    "check if value fulfilling the predicate exists" in {
      tree0.existsValue(_ == "a") shouldBe false
      tree1.existsValue(_ == "a") shouldBe true
      tree1.existsValue(_ == "b") shouldBe false
      tree2.existsValue(_ == "a") shouldBe true
      tree2.existsValue(_ == "b") shouldBe true
      tree3_1.existsValue(_ == "a") shouldBe true
      tree3_1.existsValue(_ == "b") shouldBe true
      tree3_1.existsValue(_ == "c") shouldBe true
      tree3_1.existsValue(_ == "d") shouldBe false
      tree3_2.existsValue(_ == "a") shouldBe true
      tree3_2.existsValue(_ == "b") shouldBe true
      tree3_2.existsValue(_ == "c") shouldBe true
      tree3_2.existsValue(_ == "d") shouldBe false
      tree4_1.existsValue(_ == "a") shouldBe true
      tree4_1.existsValue(_ == "b") shouldBe true
      tree4_1.existsValue(_ == "c") shouldBe true
      tree4_1.existsValue(_ == "d") shouldBe true
      tree4_1.existsValue(_ == "e") shouldBe false
      tree4_2.existsValue(_ == "a") shouldBe true
      tree4_2.existsValue(_ == "b") shouldBe true
      tree4_2.existsValue(_ == "c") shouldBe true
      tree4_2.existsValue(_ == "d") shouldBe true
      tree4_2.existsValue(_ == "e") shouldBe false
      tree4_3.existsValue(_ == "a") shouldBe true
      tree4_3.existsValue(_ == "b") shouldBe true
      tree4_3.existsValue(_ == "c") shouldBe true
      tree4_3.existsValue(_ == "d") shouldBe true
      tree4_3.existsValue(_ == "e") shouldBe false
      tree7.existsValue(_ == "a") shouldBe true
      tree7.existsValue(_ == "b") shouldBe true
      tree7.existsValue(_ == "c") shouldBe true
      tree7.existsValue(_ == "d") shouldBe true
      tree7.existsValue(_ == "e") shouldBe true
      tree7.existsValue(_ == "f") shouldBe true
      tree7.existsValue(_ == "g") shouldBe true
      tree7.existsValue(_ == "h") shouldBe false
    }

    "check if tree contains child holding a value" in {
      tree0.containsChildValue("a") shouldBe false
      tree1.containsChildValue("a") shouldBe false
      tree1.containsChildValue("b") shouldBe false
      tree2.containsChildValue("a") shouldBe false
      tree2.containsChildValue("b") shouldBe true
      tree2.containsChildValue("c") shouldBe false
      tree3_1.containsChildValue("a") shouldBe false
      tree3_1.containsChildValue("b") shouldBe true
      tree3_1.containsChildValue("c") shouldBe false
      tree3_2.containsChildValue("a") shouldBe false
      tree3_2.containsChildValue("b") shouldBe true
      tree3_2.containsChildValue("c") shouldBe true
      tree4_1.containsChildValue("a") shouldBe false
      tree4_1.containsChildValue("b") shouldBe true
      tree4_1.containsChildValue("c") shouldBe false
      tree4_1.containsChildValue("d") shouldBe false
      tree4_2.containsChildValue("a") shouldBe false
      tree4_2.containsChildValue("b") shouldBe true
      tree4_2.containsChildValue("c") shouldBe false
      tree4_2.containsChildValue("d") shouldBe true
      tree4_3.containsChildValue("a") shouldBe false
      tree4_3.containsChildValue("b") shouldBe true
      tree4_3.containsChildValue("c") shouldBe true
      tree4_3.containsChildValue("d") shouldBe true
    }

    "check if tree contains child with a value fulfilling the predicate" in {
      tree0.existsChildValue(_ == "a") shouldBe false
      tree1.existsChildValue(_ == "a") shouldBe false
      tree1.existsChildValue(_ == "b") shouldBe false
      tree2.existsChildValue(_ == "a") shouldBe false
      tree2.existsChildValue(_ == "b") shouldBe true
      tree2.existsChildValue(_ == "c") shouldBe false
      tree3_1.existsChildValue(_ == "a") shouldBe false
      tree3_1.existsChildValue(_ == "b") shouldBe true
      tree3_1.existsChildValue(_ == "c") shouldBe false
      tree3_2.existsChildValue(_ == "a") shouldBe false
      tree3_2.existsChildValue(_ == "b") shouldBe true
      tree3_2.existsChildValue(_ == "c") shouldBe true
      tree4_1.existsChildValue(_ == "a") shouldBe false
      tree4_1.existsChildValue(_ == "b") shouldBe true
      tree4_1.existsChildValue(_ == "c") shouldBe false
      tree4_1.existsChildValue(_ == "d") shouldBe false
      tree4_2.existsChildValue(_ == "a") shouldBe false
      tree4_2.existsChildValue(_ == "b") shouldBe true
      tree4_2.existsChildValue(_ == "c") shouldBe false
      tree4_2.existsChildValue(_ == "d") shouldBe true
      tree4_3.existsChildValue(_ == "a") shouldBe false
      tree4_3.existsChildValue(_ == "b") shouldBe true
      tree4_3.existsChildValue(_ == "c") shouldBe true
      tree4_3.existsChildValue(_ == "d") shouldBe true
    }

    "check if tree contains given child" in {
      tree0.containsChild(Tree("a")) shouldBe false
      tree1.containsChild(Tree("a")) shouldBe false
      tree1.containsChild(Tree("b")) shouldBe false
      tree2.containsChild(Tree("a")) shouldBe false
      tree2.containsChild(Tree("b")) shouldBe true
      tree2.containsChild(Tree("c")) shouldBe false
      tree3_1.containsChild(Tree("a")) shouldBe false
      tree3_1.containsChild(Tree("b", Tree("c"))) shouldBe true
      tree3_1.containsChild(Tree("c")) shouldBe false
      tree3_2.containsChild(Tree("a")) shouldBe false
      tree3_2.containsChild(Tree("b")) shouldBe true
      tree3_2.containsChild(Tree("c")) shouldBe true
      tree4_1.containsChild(Tree("a")) shouldBe false
      tree4_1.containsChild(Tree("b", Tree("c", Tree("d")))) shouldBe true
      tree4_1.containsChild(Tree("c", Tree("d"))) shouldBe false
      tree4_1.containsChild(Tree("d")) shouldBe false
      tree4_2.containsChild(Tree("a")) shouldBe false
      tree4_2.containsChild(Tree("b", Tree("c"))) shouldBe true
      tree4_2.containsChild(Tree("c")) shouldBe false
      tree4_2.containsChild(Tree("d")) shouldBe true
      tree4_3.containsChild(Tree("a")) shouldBe false
      tree4_3.containsChild(Tree("b")) shouldBe true
      tree4_3.containsChild(Tree("c")) shouldBe true
      tree4_3.containsChild(Tree("d")) shouldBe true
    }

    "check if tree contains child fulfilling the predicate" in {
      tree0.existsChild(_ == Tree("a")) shouldBe false
      tree1.existsChild(_ == Tree("a")) shouldBe false
      tree1.existsChild(_ == Tree("b")) shouldBe false
      tree2.existsChild(_ == Tree("a")) shouldBe false
      tree2.existsChild(_ == Tree("b")) shouldBe true
      tree2.existsChild(_ == Tree("c")) shouldBe false
      tree3_1.existsChild(_ == Tree("a")) shouldBe false
      tree3_1.existsChild(_ == Tree("b", Tree("c"))) shouldBe true
      tree3_1.existsChild(_ == Tree("c")) shouldBe false
      tree3_2.existsChild(_ == Tree("a")) shouldBe false
      tree3_2.existsChild(_ == Tree("b")) shouldBe true
      tree3_2.existsChild(_ == Tree("c")) shouldBe true
      tree4_1.existsChild(_ == Tree("a")) shouldBe false
      tree4_1.existsChild(_ == Tree("b", Tree("c", Tree("d")))) shouldBe true
      tree4_1.existsChild(_ == Tree("c")) shouldBe false
      tree4_1.existsChild(_ == Tree("d")) shouldBe false
      tree4_2.existsChild(_ == Tree("a")) shouldBe false
      tree4_2.existsChild(_ == Tree("b", Tree("c"))) shouldBe true
      tree4_2.existsChild(_ == Tree("c")) shouldBe false
      tree4_2.existsChild(_ == Tree("d")) shouldBe true
      tree4_3.existsChild(_ == Tree("a")) shouldBe false
      tree4_3.existsChild(_ == Tree("b")) shouldBe true
      tree4_3.existsChild(_ == Tree("c")) shouldBe true
      tree4_3.existsChild(_ == Tree("d")) shouldBe true

      tree0.existsChild(_.size > 2) shouldBe false
      tree1.existsChild(_.size > 2) shouldBe false
      tree2.existsChild(_.size > 2) shouldBe false
      tree3_1.existsChild(_.size > 2) shouldBe false
      tree3_2.existsChild(_.size > 2) shouldBe false
      tree4_1.existsChild(_.size > 2) shouldBe true
      tree4_2.existsChild(_.size > 2) shouldBe false
      tree4_3.existsChild(_.size > 2) shouldBe false
      tree7.existsChild(_.size > 2) shouldBe true
      tree9.existsChild(_.size > 2) shouldBe true
      tree13.existsChild(_.size > 2) shouldBe true

      tree0.existsChild(_.size   % 2 != 0) shouldBe false
      tree1.existsChild(_.size   % 2 != 0) shouldBe false
      tree2.existsChild(_.size   % 2 != 0) shouldBe true
      tree3_1.existsChild(_.size % 2 != 0) shouldBe false
      tree3_2.existsChild(_.size % 2 != 0) shouldBe true
      tree4_1.existsChild(_.size % 2 != 0) shouldBe true
      tree4_2.existsChild(_.size % 2 != 0) shouldBe true
      tree4_3.existsChild(_.size % 2 != 0) shouldBe true
      tree7.existsChild(_.size   % 2 != 0) shouldBe true
      tree9.existsChild(_.size   % 2 != 0) shouldBe true
      tree13.existsChild(_.size  % 2 != 0) shouldBe true
    }

    "check if the tree contains a branch fulfilling the predicate" in {
      tree0.existsBranch(_.toList == List()) shouldBe false
      tree0.existsBranch(_.toList == List("a")) shouldBe false
      tree0.existsBranch(_.toList == List("a", "b")) shouldBe false
      tree1.existsBranch(_.toList == List("a")) shouldBe true
      tree1.existsBranch(_.toList == List("b")) shouldBe false
      tree1.existsBranch(_.toList == List("a", "b")) shouldBe false
      tree2.existsBranch(_.toList == List("a")) shouldBe false
      tree2.existsBranch(_.toList == List("a", "b")) shouldBe true
      tree2.existsBranch(_.toList == List("a", "c")) shouldBe false
      tree2.existsBranch(_.toList == List("a", "b", "c")) shouldBe false
      tree3_1.existsBranch(_.toList == List("a", "b", "c")) shouldBe true
      tree3_1.existsBranch(_.toList == List("a", "b")) shouldBe false
      tree3_1.existsBranch(_.toList == List("a", "c")) shouldBe false
      tree3_1.existsBranch(_.toList == List("a")) shouldBe false
      tree3_1.existsBranch(_.toList == List("b")) shouldBe false
      tree3_1.existsBranch(_.toList == List("b", "c")) shouldBe false
      tree3_2.existsBranch(_.toList == List("a", "b")) shouldBe true
      tree3_2.existsBranch(_.toList == List("a", "c")) shouldBe true
      tree3_2.existsBranch(_.toList == List("a", "a")) shouldBe false
      tree3_2.existsBranch(_.toList == List("a")) shouldBe false
      tree3_2.existsBranch(_.toList == List("b")) shouldBe false
      tree3_2.existsBranch(_.toList == List("c")) shouldBe false
      tree3_2.existsBranch(_.toList == List("a", "b", "c")) shouldBe false
      tree3_2.existsBranch(_.toList == List("a", "c", "b")) shouldBe false
      tree3_2.existsBranch(_.toList == List("a", "c", "e")) shouldBe false
      tree4_1.existsBranch(_.toList == List("a", "b", "c", "d")) shouldBe true
      tree4_1.existsBranch(_.toList == List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.existsBranch(_.toList == List("a", "b", "c")) shouldBe false
      tree4_1.existsBranch(_.toList == List("a", "b")) shouldBe false
      tree4_1.existsBranch(_.toList == List("a")) shouldBe false
      tree4_2.existsBranch(_.toList == List("a", "b", "c")) shouldBe true
      tree4_2.existsBranch(_.toList == List("a", "d")) shouldBe true
      tree4_2.existsBranch(_.toList == List("a", "b", "c", "d")) shouldBe false
      tree4_2.existsBranch(_.toList == List("a", "a", "a")) shouldBe false
      tree4_2.existsBranch(_.toList == List("c", "b", "a")) shouldBe false
      tree4_2.existsBranch(_.toList == List("d", "a")) shouldBe false
      tree9.existsBranch(_.toList == List("a", "b", "c")) shouldBe false
      tree9.existsBranch(_.toList == List("a", "e", "h", "i")) shouldBe true
      tree9.existsBranch(_.toList == List("a", "e", "c")) shouldBe false
      tree9.existsBranch(_.toList == List("a", "e", "f")) shouldBe false
      tree9.existsBranch(_.toList == List("a", "e", "h")) shouldBe false
    }

    "check if the tree contains a path fulfilling the predicate" in {
      tree0.existsPath(_.toList == List()) shouldBe false
      tree0.existsPath(_.toList == List("a")) shouldBe false
      tree0.existsPath(_.toList == List("a", "b")) shouldBe false
      tree1.existsPath(_.toList == List("a")) shouldBe true
      tree1.existsPath(_.toList == List("b")) shouldBe false
      tree1.existsPath(_.toList == List("a", "b")) shouldBe false
      tree2.existsPath(_.toList == List("a")) shouldBe true
      tree2.existsPath(_.toList == List("a", "b")) shouldBe true
      tree2.existsPath(_.toList == List("a", "c")) shouldBe false
      tree2.existsPath(_.toList == List("a", "b", "c")) shouldBe false
      tree3_1.existsPath(_.toList == List("a", "b", "c")) shouldBe true
      tree3_1.existsPath(_.toList == List("a", "b")) shouldBe true
      tree3_1.existsPath(_.toList == List("a", "c")) shouldBe false
      tree3_1.existsPath(_.toList == List("a")) shouldBe true
      tree3_1.existsPath(_.toList == List("b")) shouldBe false
      tree3_1.existsPath(_.toList == List("b", "c")) shouldBe false
      tree3_2.existsPath(_.toList == List("a", "b")) shouldBe true
      tree3_2.existsPath(_.toList == List("a", "c")) shouldBe true
      tree3_2.existsPath(_.toList == List("a", "a")) shouldBe false
      tree3_2.existsPath(_.toList == List("a")) shouldBe true
      tree3_2.existsPath(_.toList == List("b")) shouldBe false
      tree3_2.existsPath(_.toList == List("c")) shouldBe false
      tree3_2.existsPath(_.toList == List("a", "b", "c")) shouldBe false
      tree3_2.existsPath(_.toList == List("a", "c", "b")) shouldBe false
      tree3_2.existsPath(_.toList == List("a", "c", "e")) shouldBe false
      tree4_1.existsPath(_.toList == List("a", "b", "c", "d")) shouldBe true
      tree4_1.existsPath(_.toList == List("a", "b", "c", "d", "e")) shouldBe false
      tree4_1.existsPath(_.toList == List("a", "b", "c")) shouldBe true
      tree4_1.existsPath(_.toList == List("a", "b")) shouldBe true
      tree4_1.existsPath(_.toList == List("a")) shouldBe true
      tree4_2.existsPath(_.toList == List("a", "b", "c")) shouldBe true
      tree4_2.existsPath(_.toList == List("a", "d")) shouldBe true
      tree4_2.existsPath(_.toList == List("a", "b", "c", "d")) shouldBe false
      tree4_2.existsPath(_.toList == List("a", "a", "a")) shouldBe false
      tree4_2.existsPath(_.toList == List("c", "b", "a")) shouldBe false
      tree4_2.existsPath(_.toList == List("d", "a")) shouldBe false
      tree9.existsPath(_.toList == List("a", "b", "c")) shouldBe true
      tree9.existsPath(_.toList == List("a", "e", "h", "i")) shouldBe true
      tree9.existsPath(_.toList == List("a", "e", "c")) shouldBe false
      tree9.existsPath(_.toList == List("a", "e", "f")) shouldBe true
      tree9.existsPath(_.toList == List("a", "e", "h")) shouldBe true
    }

    "check if the tree contains a branch fulfilling the predicate - using item extractor function" in {
      val codeF: String => Int = _.head.toInt
      tree0.existsBranch[Int](_.toList == List(), codeF) shouldBe false
      tree0.existsBranch[Int](_.toList == List(97), codeF) shouldBe false
      tree0.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree1.existsBranch[Int](_.toList == List(97), codeF) shouldBe true
      tree1.existsBranch[Int](_.toList == List(98), codeF) shouldBe false
      tree1.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree2.existsBranch[Int](_.toList == List(97), codeF) shouldBe false
      tree2.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree2.existsBranch[Int](_.toList == List(97, 99), codeF) shouldBe false
      tree2.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree3_1.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree3_1.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree3_1.existsBranch[Int](_.toList == List(97, 99), codeF) shouldBe false
      tree3_1.existsBranch[Int](_.toList == List(97), codeF) shouldBe false
      tree3_1.existsBranch[Int](_.toList == List(98), codeF) shouldBe false
      tree3_1.existsBranch[Int](_.toList == List(98, 99), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree3_2.existsBranch[Int](_.toList == List(97, 99), codeF) shouldBe true
      tree3_2.existsBranch[Int](_.toList == List(97, 97), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(97), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(98), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(99), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(97, 99, 98), codeF) shouldBe false
      tree3_2.existsBranch[Int](_.toList == List(97, 99, 101), codeF) shouldBe false
      tree4_1.existsBranch[Int](_.toList == List(97, 98, 99, 100), codeF) shouldBe true
      tree4_1.existsBranch[Int](_.toList == List(97, 98, 99, 100, 101), codeF) shouldBe false
      tree4_1.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree4_1.existsBranch[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree4_1.existsBranch[Int](_.toList == List(97), codeF) shouldBe false
      tree4_2.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree4_2.existsBranch[Int](_.toList == List(97, 100), codeF) shouldBe true
      tree4_2.existsBranch[Int](_.toList == List(97, 98, 99, 100), codeF) shouldBe false
      tree4_2.existsBranch[Int](_.toList == List(97, 97, 97), codeF) shouldBe false
      tree4_2.existsBranch[Int](_.toList == List(99, 98, 97), codeF) shouldBe false
      tree4_2.existsBranch[Int](_.toList == List(100, 97), codeF) shouldBe false
      tree9.existsBranch[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree9.existsBranch[Int](_.toList == List(97, 101, 104, 105), codeF) shouldBe true
      tree9.existsBranch[Int](_.toList == List(97, 101, 99), codeF) shouldBe false
      tree9.existsBranch[Int](_.toList == List(97, 101, 102), codeF) shouldBe false
      tree9.existsBranch[Int](_.toList == List(97, 101, 104), codeF) shouldBe false
    }

    "check if the tree contains a path fulfilling the predicate - using item extractor function" in {
      val codeF: String => Int = _.head.toInt
      tree0.existsPath[Int](_.toList == List(), codeF) shouldBe false
      tree0.existsPath[Int](_.toList == List(97), codeF) shouldBe false
      tree0.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree1.existsPath[Int](_.toList == List(97), codeF) shouldBe true
      tree1.existsPath[Int](_.toList == List(98), codeF) shouldBe false
      tree1.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe false
      tree2.existsPath[Int](_.toList == List(97), codeF) shouldBe true
      tree2.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree2.existsPath[Int](_.toList == List(97, 99), codeF) shouldBe false
      tree2.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree3_1.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree3_1.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree3_1.existsPath[Int](_.toList == List(97, 99), codeF) shouldBe false
      tree3_1.existsPath[Int](_.toList == List(97), codeF) shouldBe true
      tree3_1.existsPath[Int](_.toList == List(98), codeF) shouldBe false
      tree3_1.existsPath[Int](_.toList == List(98, 99), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree3_2.existsPath[Int](_.toList == List(97, 99), codeF) shouldBe true
      tree3_2.existsPath[Int](_.toList == List(97, 97), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(97), codeF) shouldBe true
      tree3_2.existsPath[Int](_.toList == List(98), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(99), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(97, 99, 98), codeF) shouldBe false
      tree3_2.existsPath[Int](_.toList == List(97, 99, 101), codeF) shouldBe false
      tree4_1.existsPath[Int](_.toList == List(97, 98, 99, 100), codeF) shouldBe true
      tree4_1.existsPath[Int](_.toList == List(97, 98, 99, 100, 101), codeF) shouldBe false
      tree4_1.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree4_1.existsPath[Int](_.toList == List(97, 98), codeF) shouldBe true
      tree4_1.existsPath[Int](_.toList == List(97), codeF) shouldBe true
      tree4_2.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree4_2.existsPath[Int](_.toList == List(97, 100), codeF) shouldBe true
      tree4_2.existsPath[Int](_.toList == List(97, 98, 99, 100), codeF) shouldBe false
      tree4_2.existsPath[Int](_.toList == List(97, 97, 97), codeF) shouldBe false
      tree4_2.existsPath[Int](_.toList == List(99, 98, 97), codeF) shouldBe false
      tree4_2.existsPath[Int](_.toList == List(100, 97), codeF) shouldBe false
      tree9.existsPath[Int](_.toList == List(97, 98, 99), codeF) shouldBe true
      tree9.existsPath[Int](_.toList == List(97, 101, 104, 105), codeF) shouldBe true
      tree9.existsPath[Int](_.toList == List(97, 101, 99), codeF) shouldBe false
      tree9.existsPath[Int](_.toList == List(97, 101, 102), codeF) shouldBe true
      tree9.existsPath[Int](_.toList == List(97, 101, 104), codeF) shouldBe true
    }

  }

}
