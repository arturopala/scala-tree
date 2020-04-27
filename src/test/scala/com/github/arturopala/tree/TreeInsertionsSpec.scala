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

class TreeInsertionsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "insert new value to a tree" in {
      tree0.insertValue("a") shouldBe Tree("a")
      tree1.insertValue("b") shouldBe Tree("a", Tree("b"))
      tree2.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertValue("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertValue("b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree4_1.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertValue("b") shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
    }

    "insert distinct new value to a tree" in {
      tree0.insertValueDistinct("a") shouldBe Tree("a")
      tree1.insertValueDistinct("b") shouldBe Tree("a", Tree("b"))
      tree2.insertValueDistinct("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertValueDistinct("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertValueDistinct("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree4_1.insertValueDistinct("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertValueDistinct("e") shouldBe Tree(
        "a",
        Tree("e"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      tree2.insertValueDistinct("b") shouldBe tree2
      tree3_1.insertValueDistinct("b") shouldBe tree3_1
      tree3_2.insertValueDistinct("b") shouldBe tree3_2
      tree3_2.insertValueDistinct("c") shouldBe tree3_2
      tree4_1.insertValueDistinct("b") shouldBe tree4_1
      tree4_2.insertValueDistinct("b") shouldBe tree4_2
      tree4_2.insertValueDistinct("d") shouldBe tree4_2
      tree4_3.insertValueDistinct("b") shouldBe tree4_3
      tree7.insertValueDistinct("b") shouldBe tree7
      tree7.insertValueDistinct("d") shouldBe tree7
      tree7.insertValueDistinct("g") shouldBe tree7
      tree9.insertValueDistinct("b") shouldBe tree9
      tree9.insertValueDistinct("e") shouldBe tree9
    }

    "insert new value to a tree at the specified path" in {
      tree0.insertValueAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))))
      tree2.insertValueAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertValueAt(List("a", "c"), "a") shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"))
      tree2.insertValueAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree2.insertValueAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))), Tree("b"))
      tree3_1
        .insertValueAt(List("a", "b", "c", "d"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("d", Tree("a")))))
      tree3_1.insertValueAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree3_1.insertValueAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertValueAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertValueAt(List("a"), "b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertValueAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertValueAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2.insertValueAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree3_2.insertValueAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertValueAt(List("a", "c"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("c")))
      tree3_2.insertValueAt(List("a", "c", "d"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("d", Tree("c"))))
    }

    "insert distinct new value to a tree at the specified path" in {
      tree0.insertValueDistinctAt(List(), "a") shouldBe Tree("a")
      tree0.insertValueDistinctAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueDistinctAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree1.insertValueDistinctAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertValueDistinctAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertValueDistinctAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree2.insertValueDistinctAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")))
      tree2.insertValueDistinctAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertValueDistinctAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertValueDistinctAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertValueDistinctAt(List("a", "b"), "d") shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertValueDistinctAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_2.insertValueDistinctAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b"), Tree("c"))
      tree3_2.insertValueDistinctAt(List("a"), "b") shouldBe tree3_2
      tree3_2.insertValueDistinctAt(List("a"), "c") shouldBe tree3_2
      tree3_2.insertValueDistinctAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")), Tree("c"))
      tree3_2.insertValueDistinctAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree7.insertValueDistinctAt(List("a", "b"), "c") shouldBe tree7
      tree7.insertValueDistinctAt(List("a", "b"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertValueDistinctAt(List("a", "b", "c"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertValueDistinctAt(List("a", "d", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("d"), Tree("f"))),
        Tree("g")
      )
      tree7.insertValueDistinctAt(List("a", "g", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("d")))
      )
    }

    "insert new value to a tree at the specified path with path item extractor" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertValueAt(List(97), "a", codeF) shouldBe Left(tree0)
      tree1.insertValueAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertValueAt(List(98), "b", codeF) shouldBe Left(tree1)
      tree1.insertValueAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a")))
      tree2.insertValueAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertValueAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertValueAt(List(98), "a", codeF) shouldBe Left(tree2)
      tree2.insertValueAt(List(97, 98), "a", codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertValueAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertValueAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertValueAt(List(97, 97), "c", codeF) shouldBe Left(tree2)
      tree2.insertValueAt(List(98, 97), "c", codeF) shouldBe Left(tree2)
    }

    "insert distinct new value to a tree at the specified path with path item extractor" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertValueDistinctAt(List(), "a", codeF) shouldBe Left(tree0)
      tree0.insertValueDistinctAt(List(97, 98), "a", codeF) shouldBe Left(tree0)
      tree1.insertValueDistinctAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertValueDistinctAt(List(97, 98), "c", codeF) shouldBe Left(tree1)
      tree2.insertValueDistinctAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertValueDistinctAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertValueDistinctAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertValueDistinctAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertValueDistinctAt(List(97, 98, 99), "d", codeF) shouldBe Left(tree2)
      tree3_1.insertValueDistinctAt(List(97, 98, 99), "d", codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("d"))))
      )
      tree3_1.insertValueDistinctAt(List(97, 98), "d", codeF) shouldBe Right(Tree("a", Tree("b", Tree("d"), Tree("c"))))
      tree3_1.insertValueDistinctAt(List(97), "d", codeF) shouldBe Right(Tree("a", Tree("d"), Tree("b", Tree("c"))))
      tree3_2.insertValueDistinctAt(List(97), "d", codeF) shouldBe Right(Tree("a", Tree("d"), Tree("b"), Tree("c")))
      tree3_2.insertValueDistinctAt(List(97), "b", codeF) shouldBe Right(tree3_2)
      tree3_2.insertValueDistinctAt(List(97), "c", codeF) shouldBe Right(tree3_2)
      tree3_2.insertValueDistinctAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b")), Tree("c")))
      tree3_2.insertValueDistinctAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("c")))
      tree7.insertValueDistinctAt(List(97, 98), "c", codeF) shouldBe Right(tree7)
      tree7.insertValueDistinctAt(List(97, 98), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("d"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueDistinctAt(List(97, 98, 99), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueDistinctAt(List(97, 100, 101), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("d"), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueDistinctAt(List(97, 103, 101), "d", codeF) shouldBe Left(tree7)
    }

    "insert new subtree to a tree" in {
      tree0.insertTree(Tree("a")) shouldBe Tree("a")
      tree1.insertTree(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertTree(Tree("d")) shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertTree(Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertTree(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))

      tree1.insertTree(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertTree(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")))
      tree1.insertTree(Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")))

      tree2.insertTree(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree2.insertTree(tree2) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree3_1.insertTree(tree3_1) shouldBe Tree("a", Tree("a", Tree("b", Tree("c"))), Tree("b", Tree("c")))
      tree3_2.insertTree(tree3_2) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("b"), Tree("c"))
      tree4_1.insertTree(tree4_1) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_2.insertTree(tree4_2) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d")
      )
      tree4_3.insertTree(tree4_3) shouldBe Tree(
        "a",
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree7.insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTree(tree7)
        .insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      allTrees.foldLeft(tree0)((acc, tree) => tree.insertTree(acc)) shouldBe Tree(
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

      allTrees.foldLeft(tree0)((acc, tree) => acc.insertTree(tree)) shouldBe Tree(
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
    }

    "insert distinct new subtree to a tree" in {
      tree0.insertTreeDistinct(Tree.empty) shouldBe Tree.empty
      tree0.insertTreeDistinct(Tree("a")) shouldBe Tree("a")
      tree0.insertTreeDistinct(tree9) shouldBe tree9
      tree1.insertTreeDistinct(Tree("a")) shouldBe Tree("a", Tree("a"))
      tree1.insertTreeDistinct(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeDistinct(Tree.empty) shouldBe tree1
      tree2.insertTreeDistinct(Tree("a")) shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertTreeDistinct(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree2.insertTreeDistinct(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTreeDistinct(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTreeDistinct(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertTreeDistinct(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e"), Tree("c"))
      )
      tree3_2.insertTreeDistinct(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d")), Tree("c"))
      tree3_2.insertTreeDistinct(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
      tree4_2.insertTreeDistinct(Tree("b", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d")
      )
      tree4_2.insertTreeDistinct(Tree("d", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d")))
      )
      tree7.insertTreeDistinct(Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinct(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeDistinct(tree7)
        .insertTreeDistinct(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

    }

    "insert new subtree to a tree at the specified path" in {
      tree0.insertTreeAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"))
      tree2.insertTreeAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTreeAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertTreeAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"), Tree("c"))
      tree3_2.insertTreeAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2
        .insertTreeAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"), Tree("c"))
      tree3_2.insertTreeAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertTreeAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")), Tree("d"))
      tree4_2.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertTreeAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertTreeAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertTreeAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")), Tree("f"))),
        Tree("g"))
    }

    "insert new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertTreeAt(List(), Tree.empty, codeF) shouldBe Right(Tree.empty)
      tree0.insertTreeAt(List(97), Tree.empty, codeF) shouldBe Left(Tree.empty)
      tree0.insertTreeAt(List(), Tree("b"), codeF) shouldBe Right(Tree("b"))
      tree0.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Left(Tree.empty)
      tree1.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree1)

      tree2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree2)
      tree2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a", Tree("b")))))
      tree2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree2)
      tree2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree2)

      tree3_1.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b", Tree("c"))))
      tree3_1.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_1)
      tree3_1.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"), Tree("c"))))
      tree3_1.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b"), Tree("c")))
      tree3_2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a")), Tree("c")))
      tree3_2.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree3_2)

      tree4_2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree4_2)
      tree4_2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b"),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 101), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 103, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 100, 101), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 103), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g", Tree("a", Tree("b"), Tree("c")))
        )
      )
    }

    "insert distinct new subtree to a tree at the specified path" in {
      tree0.insertTreeDistinctAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeDistinctAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeDistinctAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1
        .insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2
        .insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertTreeDistinctAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1
        .insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2.insertTreeDistinctAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2
        .insertTreeDistinctAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      tree3_2.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d"))
      tree4_2.insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertTreeDistinctAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertTreeDistinctAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeDistinctAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeDistinctAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertTreeDistinctAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")))),
        Tree("g"))
    }

    "insert distinct new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertTreeDistinctAt(List(), Tree.empty, codeF) shouldBe Right(Tree.empty)
      tree0.insertTreeDistinctAt(List(97), Tree.empty, codeF) shouldBe Left(Tree.empty)
      tree0.insertTreeDistinctAt(List(), Tree("b"), codeF) shouldBe Right(Tree("b"))
      tree0.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Left(Tree.empty)
      tree1.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree1)
      tree1.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Left(tree1)
      tree1.insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree1)

      tree2.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree2)
      tree2.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a", Tree("b")))))
      tree2.insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree2)
      tree2.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree2)

      tree3_1.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree3_1.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_1)
      tree3_1.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c"))))
      tree3_1.insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree3_2.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a")), Tree("c")))
      tree3_2.insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree3_2)

      tree4_2.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree4_2)
      tree4_2.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertTreeDistinctAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(98), Tree("b"), codeF) shouldBe Left(tree7)
      tree7.insertTreeDistinctAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertTreeDistinctAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(97, 98, 101), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeDistinctAt(List(97, 98, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeDistinctAt(List(97, 103, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeDistinctAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(97, 100, 101), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeDistinctAt(List(97, 103), tree3_2, codeF) shouldBe Right(
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

  }

}
