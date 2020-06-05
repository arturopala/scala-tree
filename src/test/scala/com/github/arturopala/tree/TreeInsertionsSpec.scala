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

import com.github.arturopala.tree.LaxTreeOps._

import scala.reflect.ClassTag

class TreeInsertionsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "insert distinct new leaf to a tree - prepend to existing" in {
      tree0.insertLeaf("a") shouldBe Tree("a")
      tree1.insertLeaf("b") shouldBe Tree("a", Tree("b"))
      tree2.insertLeaf("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree2.insertLeaf("b") shouldBe tree2
      tree3_1.insertLeaf("b") shouldBe tree3_1
      tree3_1.insertLeaf("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertLeaf("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree4_1.insertLeaf("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertLeaf("e") shouldBe Tree(
        "a",
        Tree("e"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      tree2.insertLeaf("b") shouldBe tree2
      tree3_1.insertLeaf("b") shouldBe tree3_1
      tree3_2.insertLeaf("b") shouldBe tree3_2
      tree3_2.insertLeaf("c") shouldBe tree3_2
      tree4_1.insertLeaf("b") shouldBe tree4_1
      tree4_2.insertLeaf("b") shouldBe tree4_2
      tree4_2.insertLeaf("d") shouldBe tree4_2
      tree4_3.insertLeaf("b") shouldBe tree4_3
      tree7.insertLeaf("b") shouldBe tree7
      tree7.insertLeaf("d") shouldBe tree7
      tree7.insertLeaf("g") shouldBe tree7
      tree9.insertLeaf("b") shouldBe tree9
      tree9.insertLeaf("e") shouldBe tree9
    }

    "insert distinct new leaf to a tree - append to existing" in {
      tree0.insertLeaf("a", append = true) shouldBe Tree("a")
      tree1.insertLeaf("b", append = true) shouldBe Tree("a", Tree("b"))
      tree2.insertLeaf("c", append = true) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree2.insertLeaf("b", append = true) shouldBe tree2
      tree3_1.insertLeaf("b", append = true) shouldBe tree3_1
      tree3_1.insertLeaf("d", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree3_1.insertLeaf("c", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree4_1.insertLeaf("c", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c"))
      tree7.insertLeaf("e", append = true) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"), Tree("e"))

      tree2.insertLeaf("b", append = true) shouldBe tree2
      tree3_1.insertLeaf("b", append = true) shouldBe tree3_1
      tree3_2.insertLeaf("b", append = true) shouldBe tree3_2
      tree3_2.insertLeaf("c", append = true) shouldBe tree3_2
      tree4_1.insertLeaf("b", append = true) shouldBe tree4_1
      tree4_2.insertLeaf("b", append = true) shouldBe tree4_2
      tree4_2.insertLeaf("d", append = true) shouldBe tree4_2
      tree4_3.insertLeaf("b", append = true) shouldBe tree4_3
      tree7.insertLeaf("b", append = true) shouldBe tree7
      tree7.insertLeaf("d", append = true) shouldBe tree7
      tree7.insertLeaf("g", append = true) shouldBe tree7
      tree9.insertLeaf("b", append = true) shouldBe tree9
      tree9.insertLeaf("e", append = true) shouldBe tree9
    }

    "insert lax new leaf to a tree - prepend to existing" in {
      tree0.insertLeafLax("a") shouldBe Tree("a")
      tree1.insertLeafLax("b") shouldBe Tree("a", Tree("b"))
      tree2.insertLeafLax("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertLeafLax("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertLeafLax("b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertLeafLax("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertLeafLax("c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree4_1.insertLeafLax("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertLeafLax("b") shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafLax("d") shouldBe Tree(
        "a",
        Tree("d"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
    }

    "insert lax new leaf to a tree - append to existing" in {
      tree0.insertLeafLax("a", append = true) shouldBe Tree("a")
      tree1.insertLeafLax("b", append = true) shouldBe Tree("a", Tree("b"))
      tree2.insertLeafLax("c", append = true) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_1.insertLeafLax("d", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree3_1.insertLeafLax("b", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree3_1.insertLeafLax("c", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertLeafLax("c", append = true) shouldBe Tree("a", Tree("b"), Tree("c"), Tree("c"))
      tree4_1.insertLeafLax("c", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c"))
      tree7.insertLeafLax("b", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g"),
        Tree("b")
      )
      tree7.insertLeafLax("d", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g"),
        Tree("d")
      )
    }

    "insert distinct new leaves to a tree - prepend to existing" in {
      tree0.insertLeaves(List("a", "b", "c")) shouldBe tree0
      tree0.insertLeaves(List("a")) shouldBe Tree("a")
      tree1.insertLeaves(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree2.insertLeaves(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("c"), Tree("b"))
      tree3_1.insertLeaves(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertLeaves(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree4_1.insertLeaves(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree4_2.insertLeaves(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("c"), Tree("b", Tree("c")), Tree("d"))
    }

    "insert lax new leaves to a tree - prepend to existing" in {
      tree0.insertLeavesLax(List("a", "b", "c")) shouldBe tree0
      tree0.insertLeavesLax(List("a")) shouldBe Tree("a")
      tree1.insertLeavesLax(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree2.insertLeavesLax(List("a", "b", "c")) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b"))
      tree3_1.insertLeavesLax(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertLeavesLax(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b"), Tree("c"))
      tree4_1.insertLeavesLax(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree4_2.insertLeavesLax(List("a", "b", "c")) shouldBe
        Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b", Tree("c")), Tree("d"))
    }

    "insert distinct new leaves to a tree - append to existing" in {
      tree0.insertLeaves(List("a", "b", "c"), append = true) shouldBe tree0
      tree0.insertLeaves(List("a"), append = true) shouldBe Tree("a")
      tree1.insertLeaves(List("a", "b", "c"), append = true) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree2.insertLeaves(List("a", "b", "c"), append = true) shouldBe Tree("a", Tree("b"), Tree("a"), Tree("c"))
      tree3_1
        .insertLeaves(List("a", "b", "c"), append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("a"), Tree("c"))
      tree3_2.insertLeaves(List("a", "b", "c"), append = true) shouldBe Tree("a", Tree("b"), Tree("c"), Tree("a"))
      tree4_1.insertLeaves(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("a"), Tree("c"))
      tree4_2.insertLeaves(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("a"), Tree("c"))
    }

    "insert lax new leaves to a tree - append to existing" in {
      tree0.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe tree0
      tree0.insertLeavesLax(List("a")) shouldBe Tree("a")
      tree1.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree2.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b"), Tree("a"), Tree("b"), Tree("c"))
      tree3_1.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("a"), Tree("b"), Tree("c"))
      tree3_2.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("a"), Tree("b"), Tree("c"))
      tree4_1.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("a"), Tree("b"), Tree("c"))
      tree4_2.insertLeavesLax(List("a", "b", "c"), append = true) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("a"), Tree("b"), Tree("c"))
    }

    "insert distinct new leaf to a tree at the specified path - prepend to existing" in {
      tree0.insertLeafAt(List(), "a") shouldBe Tree("a")
      tree0.insertLeafAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree1.insertLeafAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertLeafAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertLeafAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree2.insertLeafAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")))
      tree2.insertLeafAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertLeafAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertLeafAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertLeafAt(List("a", "b"), "d") shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertLeafAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_2.insertLeafAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b"), Tree("c"))
      tree3_2.insertLeafAt(List("a"), "b") shouldBe tree3_2
      tree3_2.insertLeafAt(List("a"), "c") shouldBe tree3_2
      tree3_2.insertLeafAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")), Tree("c"))
      tree3_2.insertLeafAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree7.insertLeafAt(List("a", "b"), "c") shouldBe tree7
      tree7.insertLeafAt(List("a", "b"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "b", "c"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "d", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("d"), Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "g", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("d")))
      )
    }

    "insert lax new leaf to a tree at the specified path - prepend to existing" in {
      tree0.insertLeafLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafLaxAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))))
      tree2.insertLeafLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertLeafLaxAt(List("a", "c"), "a") shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"))
      tree2.insertLeafLaxAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree2.insertLeafLaxAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))), Tree("b"))
      tree3_1
        .insertLeafLaxAt(List("a", "b", "c", "d"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("d", Tree("a")))))
      tree3_1.insertLeafLaxAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree3_1.insertLeafLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertLeafLaxAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertLeafLaxAt(List("a"), "b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertLeafLaxAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertLeafLaxAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a", "c"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("c")))
      tree3_2.insertLeafLaxAt(List("a", "c", "d"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("d", Tree("c"))))
    }

    "insert distinct new leaf to a tree at the specified path with path item extractor - prepend to existing" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertLeafAt(List(), "a", codeF, append = false) shouldBe Left(tree0)
      tree0.insertLeafAt(List(97, 98), "a", codeF, append = false) shouldBe Left(tree0)
      tree1.insertLeafAt(List(97), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertLeafAt(List(97, 98), "c", codeF, append = false) shouldBe Left(tree1)
      tree2.insertLeafAt(List(97), "a", codeF, append = false) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertLeafAt(List(97), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertLeafAt(List(97, 98), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertLeafAt(List(97, 98), "c", codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertLeafAt(List(97, 98, 99), "d", codeF, append = false) shouldBe Left(tree2)
      tree3_1.insertLeafAt(List(97, 98, 99), "d", codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("d"))))
      )
      tree3_1.insertLeafAt(List(97, 98), "d", codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("d"), Tree("c")))
      )
      tree3_1.insertLeafAt(List(97), "d", codeF, append = false) shouldBe Right(
        Tree("a", Tree("d"), Tree("b", Tree("c")))
      )
      tree3_2.insertLeafAt(List(97), "d", codeF, append = false) shouldBe Right(
        Tree("a", Tree("d"), Tree("b"), Tree("c"))
      )
      tree3_2.insertLeafAt(List(97), "b", codeF, append = false) shouldBe Right(tree3_2)
      tree3_2.insertLeafAt(List(97), "c", codeF, append = false) shouldBe Right(tree3_2)
      tree3_2.insertLeafAt(List(97, 98), "b", codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("b")), Tree("c"))
      )
      tree3_2.insertLeafAt(List(97, 98), "c", codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("c"))
      )
      tree7.insertLeafAt(List(97, 98), "c", codeF, append = false) shouldBe Right(tree7)
      tree7.insertLeafAt(List(97, 98), "d", codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("d"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 98, 99), "d", codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 100, 101), "d", codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("d"), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 103, 101), "d", codeF, append = false) shouldBe Left(tree7)
    }

    "insert lax new leaf to a tree at the specified path with path item extractor - prepend to existing" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertLeafLaxAt(List(97), "a", codeF, append = false) shouldBe Left(tree0)
      tree1.insertLeafLaxAt(List(97), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertLeafLaxAt(List(98), "b", codeF, append = false) shouldBe Left(tree1)
      tree1.insertLeafLaxAt(List(97), "a", codeF, append = false) shouldBe Right(Tree("a", Tree("a")))
      tree2.insertLeafLaxAt(List(97), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertLeafLaxAt(List(97), "a", codeF, append = false) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertLeafLaxAt(List(98), "a", codeF, append = false) shouldBe Left(tree2)
      tree2.insertLeafLaxAt(List(97, 98), "a", codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertLeafLaxAt(List(97, 98), "b", codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertLeafLaxAt(List(97, 98), "c", codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertLeafLaxAt(List(97, 97), "c", codeF, append = false) shouldBe Left(tree2)
      tree2.insertLeafLaxAt(List(98, 97), "c", codeF, append = false) shouldBe Left(tree2)
    }

    "insert distinct new leaf to a tree at the specified path - append to existing" in {
      tree0.insertLeafAt(List(), "a", append = true) shouldBe Tree("a")
      tree0.insertLeafAt(List("a", "b"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafAt(List("a"), "b", append = true) shouldBe Tree("a", Tree("b"))
      tree1.insertLeafAt(List("a", "b"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertLeafAt(List("a"), "a", append = true) shouldBe Tree("a", Tree("b"), Tree("a"))
      tree2.insertLeafAt(List("a"), "b", append = true) shouldBe Tree("a", Tree("b"))
      tree2.insertLeafAt(List("a", "b"), "b", append = true) shouldBe Tree("a", Tree("b", Tree("b")))
      tree2.insertLeafAt(List("a", "b"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertLeafAt(List("a", "b", "c"), "d", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertLeafAt(List("a", "b", "c"), "d", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertLeafAt(List("a", "b"), "d", append = true) shouldBe Tree("a", Tree("b", Tree("c"), Tree("d")))
      tree3_1.insertLeafAt(List("a"), "d", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree3_2.insertLeafAt(List("a"), "d", append = true) shouldBe Tree("a", Tree("b"), Tree("c"), Tree("d"))
      tree3_2.insertLeafAt(List("a"), "b", append = true) shouldBe tree3_2
      tree3_2.insertLeafAt(List("a"), "c", append = true) shouldBe tree3_2
      tree3_2.insertLeafAt(List("a", "b"), "b", append = true) shouldBe Tree("a", Tree("b", Tree("b")), Tree("c"))
      tree3_2.insertLeafAt(List("a", "b"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree7.insertLeafAt(List("a", "b"), "c", append = true) shouldBe tree7
      tree7.insertLeafAt(List("a", "b"), "d", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "b", "c"), "d", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "d", "e"), "d", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"), Tree("d"))),
        Tree("g")
      )
      tree7.insertLeafAt(List("a", "g", "e"), "d", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("d")))
      )
    }

    "insert lax new leaf to a tree at the specified path - append to existing" in {
      tree0.insertLeafLaxAt(List("a", "b"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafLaxAt(List("a", "b"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertLeafLaxAt(List("a", "c", "c"), "a", append = true) shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))))
      tree2.insertLeafLaxAt(List("a", "b"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertLeafLaxAt(List("a", "c"), "a", append = true) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      tree2.insertLeafLaxAt(List("a", "b", "c"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree2.insertLeafLaxAt(List("a", "c", "c"), "a", append = true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("c", Tree("a")))
      )
      tree3_1
        .insertLeafLaxAt(List("a", "b", "c", "d"), "a", append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d", Tree("a"))))
      )
      tree3_1
        .insertLeafLaxAt(List("a", "b", "c"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree3_1.insertLeafLaxAt(List("a", "b"), "a", append = true) shouldBe Tree("a", Tree("b", Tree("c"), Tree("a")))
      tree3_1.insertLeafLaxAt(List("a", "b"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertLeafLaxAt(List("a"), "b", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree3_1.insertLeafLaxAt(List("a"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a"), "c", append = true) shouldBe Tree("a", Tree("b"), Tree("c"), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a"), "a", append = true) shouldBe Tree("a", Tree("b"), Tree("c"), Tree("a"))
      tree3_2.insertLeafLaxAt(List("a", "b"), "c", append = true) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertLeafLaxAt(List("a", "c"), "c", append = true) shouldBe Tree("a", Tree("b"), Tree("c", Tree("c")))
      tree3_2.insertLeafLaxAt(List("a", "c", "d"), "c", append = true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("d", Tree("c")))
      )
    }

    "insert distinct new leaf to a tree at the specified path with path item extractor - append to existing" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertLeafAt(List(), "a", codeF, append = true) shouldBe Left(tree0)
      tree0.insertLeafAt(List(97, 98), "a", codeF, append = true) shouldBe Left(tree0)
      tree1.insertLeafAt(List(97), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertLeafAt(List(97, 98), "c", codeF, append = true) shouldBe Left(tree1)
      tree2.insertLeafAt(List(97), "a", codeF, append = true) shouldBe Right(Tree("a", Tree("b"), Tree("a")))
      tree2.insertLeafAt(List(97), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertLeafAt(List(97, 98), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertLeafAt(List(97, 98), "c", codeF, append = true) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertLeafAt(List(97, 98, 99), "d", codeF, append = true) shouldBe Left(tree2)
      tree3_1.insertLeafAt(List(97, 98, 99), "d", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("d"))))
      )
      tree3_1.insertLeafAt(List(97, 98), "d", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b", Tree("c"), Tree("d")))
      )
      tree3_1.insertLeafAt(List(97), "d", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d"))
      )
      tree3_2.insertLeafAt(List(97), "d", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      )
      tree3_2.insertLeafAt(List(97), "b", codeF, append = true) shouldBe Right(tree3_2)
      tree3_2.insertLeafAt(List(97), "c", codeF, append = true) shouldBe Right(tree3_2)
      tree3_2.insertLeafAt(List(97, 98), "b", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b", Tree("b")), Tree("c"))
      )
      tree3_2.insertLeafAt(List(97, 98), "c", codeF, append = true) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("c"))
      )
      tree7.insertLeafAt(List(97, 98), "c", codeF, append = true) shouldBe Right(tree7)
      tree7.insertLeafAt(List(97, 98), "d", codeF, append = true) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c"), Tree("d")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 98, 99), "d", codeF, append = true) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 100, 101), "d", codeF, append = true) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"), Tree("d"))),
          Tree("g")
        )
      )
      tree7.insertLeafAt(List(97, 103, 101), "d", codeF, append = true) shouldBe Left(tree7)
    }

    "insert lax new leaf to a tree at the specified path with path item extractor - append to existing" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertLeafLaxAt(List(97), "a", codeF, append = true) shouldBe Left(tree0)
      tree1.insertLeafLaxAt(List(97), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertLeafLaxAt(List(98), "b", codeF, append = true) shouldBe Left(tree1)
      tree1.insertLeafLaxAt(List(97), "a", codeF, append = true) shouldBe Right(Tree("a", Tree("a")))
      tree2.insertLeafLaxAt(List(97), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertLeafLaxAt(List(97), "a", codeF, append = true) shouldBe Right(Tree("a", Tree("b"), Tree("a")))
      tree2.insertLeafLaxAt(List(98), "a", codeF, append = true) shouldBe Left(tree2)
      tree2.insertLeafLaxAt(List(97, 98), "a", codeF, append = true) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertLeafLaxAt(List(97, 98), "b", codeF, append = true) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertLeafLaxAt(List(97, 98), "c", codeF, append = true) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertLeafLaxAt(List(97, 97), "c", codeF, append = true) shouldBe Left(tree2)
      tree2.insertLeafLaxAt(List(98, 97), "c", codeF, append = true) shouldBe Left(tree2)
    }

    "insert distinct new child to a tree" in {
      tree0.insertChild(Tree.empty) shouldBe Tree.empty
      tree0.insertChild(Tree("a")) shouldBe Tree("a")
      tree0.insertChild(tree9) shouldBe tree9
      tree1.insertChild(Tree.empty) shouldBe tree1
      tree1.insertChild(Tree("a")) shouldBe Tree("a", Tree("a"))
      tree1.insertChild(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertChild(Tree("b").deflated) shouldBe Tree("a", Tree("b"))
      tree1.insertChild(Tree("b", Tree("c")).deflated) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertChild(Tree.empty) shouldBe tree1
      tree2.insertChild(Tree("a")) shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertChild(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree2.insertChild(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertChild(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertChild(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertChild(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e"), Tree("c"))
      )
      tree3_2.insertChild(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d")), Tree("c"))
      tree3_2.insertChild(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
      tree4_2.insertChild(Tree("b", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d")
      )
      tree4_2.insertChild(Tree("d", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d")))
      )
      tree7.insertChild(Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChild(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertChild(tree7)
        .insertChild(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .insertChild(Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x"), Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "insert lax new child to a tree" in {
      tree0.insertChildLax(Tree.empty) shouldBe tree0
      tree0.insertChildLax(Tree("a")) shouldBe Tree("a")
      tree1.insertChild(Tree.empty) shouldBe tree1
      tree1.insertChildLax(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertChildLax(Tree("b").deflated) shouldBe Tree("a", Tree("b"))
      tree1.insertChildLax(Tree("b", Tree("c")).deflated) shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertChildLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertChildLax(Tree("d")) shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertChildLax(Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertChildLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertChildLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))

      tree1.insertChildLax(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertChildLax(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")))
      tree1.insertChildLax(Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")))

      tree2.insertChildLax(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree2.insertChildLax(tree2) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree3_1.insertChildLax(tree3_1) shouldBe Tree("a", Tree("a", Tree("b", Tree("c"))), Tree("b", Tree("c")))
      tree3_2.insertChildLax(tree3_2) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("b"), Tree("c"))
      tree4_1.insertChildLax(tree4_1) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_2.insertChildLax(tree4_2) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d")
      )
      tree4_3.insertChildLax(tree4_3) shouldBe Tree(
        "a",
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree7.insertChildLax(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertChildLax(tree7)
        .insertChildLax(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      allTrees.foldLeft(tree0)((acc, tree) => tree.insertChildLax(acc)) shouldBe Tree(
        "a",
        Tree(
          "a",
          Tree(
            "a",
            Tree(
              "a",
              Tree(
                "a",
                Tree("a", Tree("a", Tree("a", Tree("a"), Tree("b")), Tree("b", Tree("c"))), Tree("b"), Tree("c")),
                Tree("b", Tree("c", Tree("d")))
              ),
              Tree("b", Tree("c")),
              Tree("d")
            ),
            Tree("b"),
            Tree("c"),
            Tree("d")
          ),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        ),
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )

      allTrees.foldLeft(tree0)((acc, tree) => acc.insertChildLax(tree)) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("a", Tree("b"), Tree("c")),
        Tree("a", Tree("b", Tree("c"))),
        Tree("a", Tree("b"))
      )

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .insertChildLax(Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x")), Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "insert distinct new subtree to a tree at the specified path" in {
      tree0.insertChildAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertChildAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertChildAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertChildAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertChildAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertChildAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1
        .insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertChildAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertChildAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertChildAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2
        .insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertChildAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertChildAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertChildAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertChildAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1
        .insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2.insertChildAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2
        .insertChildAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      tree3_2.insertChildAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertChildAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertChildAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertChildAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertChildAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertChildAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertChildAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d")
      )
      tree4_2.insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertChildAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertChildAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertChildAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertChildAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertChildAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")))),
        Tree("g")
      )
    }

    "insert lax new subtree to a tree at the specified path" in {
      tree0.insertTreeLaxAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeLaxAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeLaxAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"))
      tree2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertTreeLaxAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2
        .insertTreeLaxAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"), Tree("c"))
      tree3_2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d")
      )
      tree4_2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertTreeLaxAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")), Tree("f"))),
        Tree("g")
      )
    }

    "insert distinct new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertChildAt(List(), Tree.empty, codeF, append = false) shouldBe Right(Tree.empty)
      tree0.insertChildAt(List(97), Tree.empty, codeF, append = false) shouldBe Left(Tree.empty)
      tree0.insertChildAt(List(), Tree("b"), codeF, append = false) shouldBe Right(Tree("b"))
      tree0.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Left(Tree.empty)
      tree1.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree1)
      tree1.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Left(tree1)
      tree1.insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Left(tree1)
      tree1.insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree1)
      tree1.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree1)

      tree2.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree2)
      tree2.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))))
      )
      tree2.insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree2)
      tree2.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree2)

      tree3_1.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree3_1.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree3_1)
      tree3_1.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")))
      )
      tree3_1.insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree3_2.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree3_2)
      tree3_2.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a")), Tree("c"))
      )
      tree3_2.insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree3_2)
      tree3_2.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree3_2)

      tree4_2.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree4_2)
      tree4_2.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertChildAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree7)
      tree7.insertChildAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertChildAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(97, 98, 101), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertChildAt(List(97, 98, 101, 102), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertChildAt(List(97, 103, 101, 102), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertChildAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(97, 100, 101), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertChildAt(List(97, 103), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g", Tree("a", Tree("b"), Tree("c")))
        )
      )
    }

    "insert lax new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertTreeLaxAt(List(), Tree.empty, codeF, append = false) shouldBe Right(Tree.empty)
      tree0.insertTreeLaxAt(List(97), Tree.empty, codeF, append = false) shouldBe Left(Tree.empty)
      tree0.insertTreeLaxAt(List(), Tree("b"), codeF, append = false) shouldBe Right(Tree("b"))
      tree0.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Left(Tree.empty)
      tree1.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree1)

      tree2.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree2)
      tree2.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))))
      )
      tree2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree2)
      tree2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree2)

      tree3_1.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("c")))
      )
      tree3_1.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree3_1)
      tree3_1.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")))
      )
      tree3_1.insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree("a", Tree("b"), Tree("b"), Tree("c"))
      )
      tree3_2.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree3_2)
      tree3_2.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a")), Tree("c"))
      )
      tree3_2.insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Left(tree3_2)
      tree3_2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Left(tree3_2)

      tree4_2.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree4_2)
      tree4_2.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertTreeLaxAt(List(97), Tree("b"), codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b"),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(98), Tree("b"), codeF, append = false) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98), tree1, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertTreeLaxAt(List(97, 98), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 99), tree2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 101), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98, 101, 102), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 103, 101, 102), tree3_2, codeF, append = false) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 100, 101), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 103), tree3_2, codeF, append = false) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g", Tree("a", Tree("b"), Tree("c")))
        )
      )
    }

    "insert new branch to a tree" in {
      tree0.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("a", "c", "c", "d")) shouldBe Tree("a", Tree("c", Tree("c", Tree("d"))))
      tree1.insertBranch(List("b", "c", "d")) shouldBe Tree("a")
      tree2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree2.insertBranch(List("a", "c", "c", "d")) shouldBe Tree("a", Tree("c", Tree("c", Tree("d"))), Tree("b"))
      tree2
        .insertBranch(List("a", "b", "c", "d"))
        .insertBranch(List("a", "b", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f")), Tree("c", Tree("d")))
      )
      tree3_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertBranch(List("a", "b", "e", "f")) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c")))
      tree3_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c"))
      tree4_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree4_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("d"))
      tree4_3.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("c"),
        Tree("d")
      )
      tree7.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "g", "h", "i")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("h", Tree("i")))
      )
      tree7.insertBranch(List("a", "d", "g", "h")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("g", Tree("h")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "d", "e", "g")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("g"), Tree("f"))),
        Tree("g")
      )
    }

    "insert distinct new children" in {
      tree0.insertChildren(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe tree0
      tree0.insertChildren(List(Tree("a"))) shouldBe Tree("a")
      tree0.insertChildren(List(Tree("a", Tree("b")))) shouldBe Tree("a", Tree("b"))
      tree1.insertChildren(List(Tree("a"))) shouldBe Tree("a", Tree("a"))
      tree1.insertChildren(List(Tree.empty)) shouldBe tree1
      tree1.insertChildren(List(Tree.empty, Tree.empty, Tree.empty)) shouldBe tree1
      tree1.insertChildren(List(Tree.empty, Tree("a"), Tree.empty, Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      tree1.insertChildren(List(Tree.empty, Tree("a", Tree("c")).deflated, Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("a", Tree("c")), Tree("b"))
      tree1.insertChildren(List(Tree.empty, Tree("b"), Tree.empty, Tree("b"))) shouldBe Tree("a", Tree("b"))
      tree1.insertChildren(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree1.insertChildren(List(Tree("b"), Tree("b"))) shouldBe Tree("a", Tree("b"))
      tree1.insertChildren(List(Tree("b", Tree("c")), Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertChildren(List(Tree("b", Tree("c")), Tree("b", Tree("d")))) shouldBe
        Tree("a", Tree("b", Tree("c"), Tree("d")))
      tree1.insertChildren(List(Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("d"))))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertChildren(List(Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("e"))))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))))
      tree1.insertChildren(List(Tree("b", Tree("c", Tree("d")), Tree("f")), Tree("b", Tree("c", Tree("e")), Tree("g")))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e")), Tree("f"), Tree("g")))
      tree1.insertChildren(List(Tree("a"), Tree("b"), Tree("a"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      tree1.insertChildren(
        List(
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      ) shouldBe
        Tree(
          "a",
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      tree2.insertChildren(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("c"), Tree("b"))
      tree2.insertChildren(List(Tree("b"), Tree("c"), Tree("b"))) shouldBe Tree("a", Tree("c"), Tree("b"))
      tree2.insertChildren(
        List(
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      ) shouldBe
        Tree(
          "a",
          Tree("a", Tree("d"), Tree("e")),
          Tree("c", Tree("h", Tree("i")), Tree("j")),
          Tree("b", Tree("f", Tree("g")))
        )
    }

    "insert lax new children" in {
      tree0.insertChildrenLax(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe tree0
      tree0.insertChildrenLax(List(Tree("a"))) shouldBe Tree("a")
      tree0.insertChildrenLax(List(Tree("a", Tree("b")))) shouldBe Tree("a", Tree("b"))
      tree1.insertChildrenLax(List(Tree.empty)) shouldBe tree1
      tree1.insertChildrenLax(List(Tree.empty, Tree.empty, Tree.empty)) shouldBe tree1
      tree1.insertChildrenLax(List(Tree.empty, Tree("a"), Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("a"), Tree("b"))
      tree1.insertChildrenLax(List(Tree.empty, Tree("b"), Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("b"), Tree("b"))
      tree1.insertChildrenLax(List(Tree.empty, Tree("b", Tree("c")).deflated, Tree.empty, Tree("b"))) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree1.insertChildrenLax(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree1.insertChildrenLax(List(Tree("a"), Tree("b"), Tree("a"))) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("a"))
      tree1.insertChildrenLax(
        List(
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      ) shouldBe
        Tree(
          "a",
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      tree2.insertChildrenLax(List(Tree("a"), Tree("b"), Tree("c"))) shouldBe
        Tree("a", Tree("a"), Tree("b"), Tree("c"), Tree("b"))
      tree2.insertChildrenLax(List(Tree("b"), Tree("c"), Tree("b"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("b"),
        Tree("b")
      )
      tree2.insertChildrenLax(
        List(
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j"))
        )
      ) shouldBe
        Tree(
          "a",
          Tree("a", Tree("d"), Tree("e")),
          Tree("b", Tree("f", Tree("g"))),
          Tree("c", Tree("h", Tree("i")), Tree("j")),
          Tree("b")
        )
    }

  }

}
