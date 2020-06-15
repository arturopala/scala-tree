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

import com.github.arturopala.tree.internal.NodeTree._

class NodeTreeSpec extends AnyWordSpecCompat {

  def all[T]: T => Boolean = _ => true
  def none[T]: T => Boolean = _ => false
  val even: String => Boolean = s => s.head.toInt % 2 == 0
  val odd: String => Boolean = s => s.head.toInt  % 2 != 0

  s"NodeTree" should {

    "convert NodeTree to arrays" in {
      val (s1, v1) = Tree("a").toArrays
      s1 shouldBe Array(0)
      v1 shouldBe Array("a")
      val (s2, v2) = Tree(1, Tree(2, Tree(3), Tree(4)), Tree(5)).toArrays
      s2 shouldBe Array(0, 0, 0, 2, 2)
      v2 shouldBe Array(5, 4, 3, 2, 1)
      val (s3, v3) = Tree(1d, Tree(4d), Tree(2d, Tree(3d)), Tree(5d)).toArrays
      s3 shouldBe Array(0, 0, 1, 0, 3)
      v3 shouldBe Array(5d, 3d, 2d, 4d, 1d)
    }

    "insert branch" in {
      insertBranch(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"))),
        List("a", "c", "d", "e", "f").iterator,
        append = false
      ) shouldBe
        Some(Tree("a", Tree("b"), Tree("c", Tree("d", Tree("e", Tree("f")))), Tree("d", Tree("e"))))
      insertBranch(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"))),
        List("a", "c", "d", "e", "f").iterator,
        append = true
      ) shouldBe
        Some(Tree("a", Tree("b"), Tree("c", Tree("d", Tree("e", Tree("f")))), Tree("d", Tree("e"))))
      insertBranch(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"))),
        List("a", "d", "d", "e", "f").iterator,
        append = false
      ) shouldBe
        Some(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("d", Tree("e", Tree("f"))), Tree("e"))))
      insertBranch(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"))),
        List("a", "d", "d", "e", "f").iterator,
        append = true
      ) shouldBe
        Some(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"), Tree("d", Tree("e", Tree("f"))))))
    }

    "insert branch unsafe" in {
      insertBranchUnsafe(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e"))), List("a", "c", "d", "e", "f").iterator) shouldBe
        Some(Tree("a", Tree("b"), Tree("c", Tree("d", Tree("e", Tree("f")))), Tree("d", Tree("e"))))
    }

    "iterate over tree values depth-first" in {
      valuesIterator(Tree("a"), true).toList shouldBe List("a")
      valuesIterator(Tree("a", Tree("b")), true).toList shouldBe List("a", "b")
      valuesIterator(Tree("a", Tree("b"), Tree("c")), true).toList shouldBe List("a", "b", "c")
      valuesIterator(Tree("a", Tree("b", Tree("c")), Tree("d")), true).toList shouldBe List("a", "b", "c", "d")
      valuesIterator(Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), true).toList shouldBe
        List("a", "b", "c", "d", "e")
      valuesIterator(
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        true
      ).toList shouldBe
        List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "iterate over tree values breadth-first" in {
      valuesIterator(Tree("a"), false).toList shouldBe List("a")
      valuesIterator(Tree("a", Tree("b")), false).toList shouldBe List("a", "b")
      valuesIterator(Tree("a", Tree("b"), Tree("c")), false).toList shouldBe List("a", "b", "c")
      valuesIterator(Tree("a", Tree("b", Tree("c")), Tree("d")), false).toList shouldBe List("a", "b", "d", "c")
      valuesIterator(Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), false).toList shouldBe
        List("a", "b", "d", "c", "e")
      valuesIterator(
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        false
      ).toList shouldBe
        List("a", "b", "f", "c", "e", "g", "h", "d", "i")
    }

    "iterate over tree values depth-first with filter" in {
      val pred = Set("a", "c", "d", "f", "g", "i").contains _
      valuesIteratorWithFilter(pred, Tree("a"), true).toList shouldBe List("a")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b")), true).toList shouldBe List("a")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b"), Tree("c")), true).toList shouldBe List("a", "c")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), true).toList shouldBe
        List("a", "c", "d")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), true).toList shouldBe
        List("a", "c", "d")
      valuesIteratorWithFilter(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        true
      ).toList shouldBe
        List("a", "c", "d", "f", "g", "i")
    }

    "iterate over tree values breadth-first with filter" in {
      val pred = Set("b", "c", "e", "f", "h", "g").contains _
      valuesIteratorWithFilter(pred, Tree("a"), false).toList shouldBe List()
      valuesIteratorWithFilter(pred, Tree("a", Tree("b")), false).toList shouldBe List("b")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b"), Tree("c")), false).toList shouldBe List("b", "c")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), false).toList shouldBe List("b", "c")
      valuesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), false).toList shouldBe
        List("b", "c", "e")
      valuesIteratorWithFilter(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        false
      ).toList shouldBe
        List("b", "f", "c", "e", "g", "h")
    }

    "iterate over tree values depth-first with filter and depth limit" in {
      val pred = Set("a", "c", "d", "f", "g", "i").contains _
      valuesIteratorWithLimit(pred, Tree("a"), 2, true).toList shouldBe List("a")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b")), 2, true).toList shouldBe List("a")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b"), Tree("c")), 2, true).toList shouldBe List("a", "c")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), 2, true).toList shouldBe
        List("a", "d")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), 2, true).toList shouldBe
        List("a", "d")
      valuesIteratorWithLimit(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        3,
        true
      ).toList shouldBe
        List("a", "c", "f", "g")
    }

    "iterate over tree values breadth-first with filter and limit" in {
      val pred = Set("b", "c", "e", "f", "h", "g").contains _
      valuesIteratorWithLimit(pred, Tree("a"), 2, false).toList shouldBe List()
      valuesIteratorWithLimit(pred, Tree("a", Tree("b")), 2, false).toList shouldBe List("b")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b"), Tree("c")), 2, false).toList shouldBe List("b", "c")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), 2, false).toList shouldBe List("b")
      valuesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), 2, false).toList shouldBe
        List("b")
      valuesIteratorWithLimit(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        2,
        false
      ).toList shouldBe
        List("b", "f")
    }

    "iterate over trees depth-first" in {
      treesIterator(Tree("a"), true).toList shouldBe List(Tree("a"))
      treesIterator(Tree("a", Tree("b")), true).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treesIterator(Tree("a", Tree("b"), Tree("c")), true).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d")), true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), true).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))),
          Tree("b", Tree("c")),
          Tree("c"),
          Tree("d", Tree("e")),
          Tree("e")
        )
      treesIterator(
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        true
      ).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
          Tree("b", Tree("c", Tree("d")), Tree("e")),
          Tree("c", Tree("d")),
          Tree("d"),
          Tree("e"),
          Tree("f", Tree("g"), Tree("h", Tree("i"))),
          Tree("g"),
          Tree("h", Tree("i")),
          Tree("i")
        )
    }

    "iterate over trees depth-first with filter" in {
      treesIterator(Tree("a"), true).toList shouldBe List(Tree("a"))
      treesIterator(Tree("a", Tree("b")), true).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treesIterator(Tree("a", Tree("b"), Tree("c")), true).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d")), true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), true).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))),
          Tree("b", Tree("c")),
          Tree("c"),
          Tree("d", Tree("e")),
          Tree("e")
        )
      treesIterator(
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        true
      ).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
          Tree("b", Tree("c", Tree("d")), Tree("e")),
          Tree("c", Tree("d")),
          Tree("d"),
          Tree("e"),
          Tree("f", Tree("g"), Tree("h", Tree("i"))),
          Tree("g"),
          Tree("h", Tree("i")),
          Tree("i")
        )
    }

    "iterate over trees breadth-first" in {
      treesIterator(Tree("a"), false).toList shouldBe List(Tree("a"))
      treesIterator(Tree("a", Tree("b")), false).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treesIterator(Tree("a", Tree("b"), Tree("c")), false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d")), false).toList shouldBe List(
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d"),
        Tree("c")
      )
      treesIterator(Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), false).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))),
          Tree("b", Tree("c")),
          Tree("d", Tree("e")),
          Tree("c"),
          Tree("e")
        )
      treesIterator(
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        false
      ).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
          Tree("b", Tree("c", Tree("d")), Tree("e")),
          Tree("f", Tree("g"), Tree("h", Tree("i"))),
          Tree("c", Tree("d")),
          Tree("e"),
          Tree("g"),
          Tree("h", Tree("i")),
          Tree("d"),
          Tree("i")
        )
    }

    "iterate over trees depth-first with filter" in {
      val pred: Tree[String] => Boolean = t => t.size % 2 == 0 || t.isLeaf
      treesIteratorWithFilter(pred, Tree("a"), true).toList shouldBe List(Tree("a"))
      treesIteratorWithFilter(pred, Tree("a", Tree("b")), true).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treesIteratorWithFilter(pred, Tree("a", Tree("b"), Tree("c")), true).toList shouldBe List(Tree("b"), Tree("c"))
      treesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      treesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), true).toList shouldBe
        List(
          Tree("b", Tree("c")),
          Tree("c"),
          Tree("d", Tree("e")),
          Tree("e")
        )
      treesIteratorWithFilter(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        true
      ).toList shouldBe
        List(
          Tree("b", Tree("c", Tree("d")), Tree("e")),
          Tree("c", Tree("d")),
          Tree("d"),
          Tree("e"),
          Tree("f", Tree("g"), Tree("h", Tree("i"))),
          Tree("g"),
          Tree("h", Tree("i")),
          Tree("i")
        )
    }

    "iterate over trees breadth-first with filter" in {
      val pred: Tree[String] => Boolean = t => t.size % 2 != 0
      treesIteratorWithFilter(pred, Tree("a"), false).toList shouldBe List(Tree("a"))
      treesIteratorWithFilter(pred, Tree("a", Tree("b")), false).toList shouldBe List(Tree("b"))
      treesIteratorWithFilter(pred, Tree("a", Tree("b"), Tree("c")), false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), false).toList shouldBe List(
        Tree("d"),
        Tree("c")
      )
      treesIteratorWithFilter(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), false).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))),
          Tree("c"),
          Tree("e")
        )
      treesIteratorWithFilter(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        false
      ).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
          Tree("e"),
          Tree("g"),
          Tree("d"),
          Tree("i")
        )
    }

    "iterate over trees depth-first with filter and depth limit" in {
      val pred: Tree[String] => Boolean = t => t.size % 2 == 0 || t.isLeaf
      treesIteratorWithLimit(pred, Tree("a"), 2, true).toList shouldBe List(Tree("a"))
      treesIteratorWithLimit(pred, Tree("a", Tree("b")), 2, true).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treesIteratorWithLimit(pred, Tree("a", Tree("b"), Tree("c")), 2, true).toList shouldBe List(Tree("b"), Tree("c"))
      treesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), 2, true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d")
      )
      treesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), 2, true).toList shouldBe
        List(
          Tree("b", Tree("c")),
          Tree("d", Tree("e"))
        )
      treesIteratorWithLimit(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        3,
        true
      ).toList shouldBe
        List(
          Tree("b", Tree("c", Tree("d")), Tree("e")),
          Tree("c", Tree("d")),
          Tree("e"),
          Tree("f", Tree("g"), Tree("h", Tree("i"))),
          Tree("g"),
          Tree("h", Tree("i"))
        )
    }

    "iterate over trees breadth-first with filter and depth limit" in {
      val pred: Tree[String] => Boolean = t => t.size % 2 != 0
      treesIteratorWithLimit(pred, Tree("a"), 2, false).toList shouldBe List(Tree("a"))
      treesIteratorWithLimit(pred, Tree("a", Tree("b")), 2, false).toList shouldBe List(Tree("b"))
      treesIteratorWithLimit(pred, Tree("a", Tree("b"), Tree("c")), 2, false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d")), 2, false).toList shouldBe List(
        Tree("d")
      )
      treesIteratorWithLimit(pred, Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e"))), 2, false).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e")))
        )
      treesIteratorWithLimit(
        pred,
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
        3,
        false
      ).toList shouldBe
        List(
          Tree("a", Tree("b", Tree("c", Tree("d")), Tree("e")), Tree("f", Tree("g"), Tree("h", Tree("i")))),
          Tree("e"),
          Tree("g")
        )
    }

    "insert new child distinct between siblings" in {
      insertDistinctBetweenSiblings(List(), Tree("a"), List(), false) shouldBe
        (List(Tree("a")), List())
      insertDistinctBetweenSiblings(List(Tree("a")), Tree("b"), List(Tree("c")), false) shouldBe
        (List(Tree("a"), Tree("b")), List(Tree("c")))
      insertDistinctBetweenSiblings(List(Tree("a", Tree("b"))), Tree("a", Tree("c")), List(Tree("a", Tree("d"))), false) shouldBe
        (List(Tree("a", Tree("b"), Tree("c"))), List(Tree("a", Tree("d"))))
      insertDistinctBetweenSiblings(List(), Tree("c"), List(Tree("c"), Tree("e")), false) shouldBe
        (List(Tree("c")), List(Tree("e")))
      insertDistinctBetweenSiblings(
        List(Tree("a", Tree("b"), Tree("c"))),
        Tree("b", Tree("c", Tree("d"))),
        List(),
        false
      ) shouldBe
        (List(Tree("a", Tree("b"), Tree("c")), Tree("b", Tree("c", Tree("d")))), List())
      insertDistinctBetweenSiblings(
        List(
          Tree("a", Tree("b")),
          Tree("a", Tree("c")),
          Tree("a", Tree("b"), Tree("c")),
          Tree("b", Tree("c", Tree("d"))),
          Tree("c", Tree("d"), Tree("e"))
        ),
        Tree("a", Tree("c", Tree("d"))),
        List(),
        false
      ) shouldBe
        (List(
          Tree("a", Tree("b")),
          Tree("a", Tree("c")),
          Tree("a", Tree("b"), Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("d"))),
          Tree("c", Tree("d"), Tree("e"))
        ), List())
    }

    "insert children distinct before siblings" in {
      insertChildrenBeforeDistinct(List(Tree("a"), Tree("b")), List(Tree("c"), Tree("d")), true) shouldBe
        List(Tree("a"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenBeforeDistinct(List(Tree("a"), Tree("b")), List(Tree("c"), Tree("b")), true) shouldBe
        List(Tree("a"), Tree("c"), Tree("b"))
      insertChildrenBeforeDistinct(
        List(Tree("a", Tree("b")), Tree("a", Tree("c"))),
        List(Tree("a", Tree("d")), Tree("a", Tree("e"))),
        true
      ) shouldBe
        List(Tree("a", Tree("b"), Tree("c"), Tree("d")), Tree("a", Tree("e")))
      insertChildrenBeforeDistinct(
        List(Tree("a", Tree("b", Tree("c"))), Tree("a", Tree("b", Tree("d")))),
        List(Tree("a", Tree("b", Tree("e"))), Tree("a", Tree("b", Tree("f")))),
        true
      ) shouldBe
        List(Tree("a", Tree("b", Tree("c"), Tree("d"), Tree("e"))), Tree("a", Tree("b", Tree("f"))))
      insertChildrenBeforeDistinct(
        List(Tree("a", Tree("b", Tree("c"))), Tree("a", Tree("b", Tree("d")))),
        List(Tree("a", Tree("c", Tree("e"))), Tree("a", Tree("b", Tree("f")))),
        true
      ) shouldBe
        List(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("c", Tree("e"))), Tree("a", Tree("b", Tree("f"))))
      insertChildrenBeforeDistinct(
        List(Tree("a", Tree("b", Tree("c"))), Tree("a", Tree("b", Tree("d")))),
        List(Tree("b", Tree("a", Tree("e"))), Tree("a", Tree("b", Tree("f")))),
        true
      ) shouldBe
        List(Tree("b", Tree("a", Tree("e"))), Tree("a", Tree("b", Tree("c"), Tree("d"), Tree("f"))))
      insertChildrenBeforeDistinct(
        List(Tree("b", Tree("c", Tree("e"))), Tree("b", Tree("c", Tree("f")))),
        List(
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("d")))
        ),
        true
      ) shouldBe
        List(
          Tree("b", Tree("c", Tree("e"), Tree("f"), Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("d")))
        )
    }

    "insert children distinct after siblings" in {
      insertChildrenAfterDistinct(
        List(
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("d")))
        ),
        List(Tree("b", Tree("c", Tree("e"))), Tree("b", Tree("c", Tree("f"))))
      ) shouldBe
        List(
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("d"), Tree("e"), Tree("f")))
        )
    }

    "insert children distinct" in {
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c"), Tree("c"), Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c"), Tree("e"), Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("e"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("c"), Tree("d")),
        List(Tree("c"), Tree("e")),
        List(Tree("c"), Tree("f")),
        false
      ) shouldBe
        Tree("a", Tree("c"), Tree("d"), Tree("e"), Tree("c"), Tree("f"))
      insertChildrenDistinct(
        "a",
        List(Tree("c"), Tree("e")),
        List(Tree("c"), Tree("d")),
        List(Tree("c"), Tree("f")),
        false
      ) shouldBe
        Tree("a", Tree("c"), Tree("e"), Tree("d"), Tree("c"), Tree("f"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("y"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("y"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("c"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("a"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("a"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b", Tree("e")), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("b", Tree("f")), Tree("z")),
        List(Tree("b", Tree("g")), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree(
          "a",
          Tree("b", Tree("e"), Tree("f")),
          Tree("c"),
          Tree("d"),
          Tree("x"),
          Tree("z"),
          Tree("b", Tree("g")),
          Tree("c"),
          Tree("d")
        )
      insertChildrenDistinct(
        "a",
        List(Tree("b", Tree("e")), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("b", Tree("f")), Tree("b", Tree("g"))),
        List(Tree("b", Tree("h")), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree(
          "a",
          Tree("b", Tree("e"), Tree("f"), Tree("g")),
          Tree("c"),
          Tree("d"),
          Tree("x"),
          Tree("b", Tree("h")),
          Tree("c"),
          Tree("d")
        )
    }

    "insert child distinct" in {
      insertChildDistinct("a", List(Tree("b")), Tree("c"), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildDistinct("a", List(Tree("b")), Tree("c"), List(Tree("c")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"))
      insertChildDistinct("a", List(Tree("c")), Tree("c"), List(Tree("d")), false) shouldBe
        Tree("a", Tree("c"), Tree("d"))
      insertChildDistinct("a", List(Tree("b")), Tree("c", Tree("e")), List(Tree("d"), Tree("c", Tree("f"))), false) shouldBe
        Tree("a", Tree("b"), Tree("c", Tree("e"), Tree("f")), Tree("d"))
      insertChildDistinct(
        "a",
        List(Tree("b")),
        Tree("c", Tree("e")),
        List(Tree("d"), Tree("c", Tree("f")), Tree("c", Tree("g"))),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c", Tree("e"), Tree("f")), Tree("d"), Tree("c", Tree("g")))
      insertChildDistinct(
        "a",
        List(Tree("c", Tree("e")), Tree("b")),
        Tree("c", Tree("f")),
        List(Tree("d"), Tree("c", Tree("g"))),
        false
      ) shouldBe
        Tree("a", Tree("c", Tree("e"), Tree("f")), Tree("b"), Tree("d"), Tree("c", Tree("g")))
      insertChildDistinct(
        "a",
        List(Tree("c", Tree("e")), Tree("c", Tree("f")), Tree("b")),
        Tree("c", Tree("g")),
        List(Tree("d"), Tree("c", Tree("h"))),
        false
      ) shouldBe
        Tree("a", Tree("c", Tree("e")), Tree("c", Tree("f"), Tree("g")), Tree("b"), Tree("d"), Tree("c", Tree("h")))
    }

    "ensure child is distinct" in {
      ensureChildDistinct(Tree("a"), 0, false) shouldBe Tree("a")
      ensureChildDistinct(Tree("a"), 1, false) shouldBe Tree("a")
      ensureChildDistinct(Tree("a", Tree("b")), 0, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b")), 1, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 0, false) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 1, false) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 0, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 1, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 2, false) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b"), Tree("b")), 2, false) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 2, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("b", Tree("d"), Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 1, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d")),
        Tree("b", Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 0, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d")),
        Tree("b", Tree("e"))
      )
    }

    "make tree distinct" in {
      makeTreeDistinct(Tree("a")) shouldBe Tree("a")
      makeTreeDistinct(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")))) shouldBe Tree("a", Tree("b", Tree("c"), Tree("d")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")))) shouldBe Tree("a", Tree("b", Tree("c"), Tree("d")))
      makeTreeDistinct(Tree("a", Tree("b"), Tree("b"))) shouldBe Tree("a", Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d"))
      )
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("b", Tree("c"), Tree("e")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d"), Tree("e"))
      )
      makeTreeDistinct(Tree("a", Tree("b", Tree("e"), Tree("c")), Tree("b", Tree("c"), Tree("e")))) shouldBe Tree(
        "a",
        Tree("b", Tree("e"), Tree("c"))
      )
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("g")))
        )
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("j")), Tree("e", Tree("g")))
        )
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("i"), Tree("j"))))
      makeTreeDistinct(Tree("a", Tree("c"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("c"), Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b"), Tree("c"), Tree("b"))) shouldBe Tree("a", Tree("b"), Tree("c"))
      makeTreeDistinct(
        Tree("a", Tree("b", Tree("c", Tree("d", Tree("e", Tree("f"), Tree("g"), Tree("f"), Tree("g"))))))
      ) shouldBe Tree("a", Tree("b", Tree("c", Tree("d", Tree("e", Tree("f"), Tree("g"))))))
    }

    "make tree distinct up to specified level" in {
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 0
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 1
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 2
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 3
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
          Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
        ),
        maxLookupLevel = 1
      ) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
        Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
      )
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
          Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
        ),
        maxLookupLevel = 2
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g"))), Tree("c", Tree("d", Tree("i"), Tree("j"))))
    }

    "build tree from triples list" in {
      buildTreeFromPartials(Vector(), Nil) shouldBe Nil
      buildTreeFromPartials(Vector((0, "a", Nil)), Nil) shouldBe List(Tree("a"))
      buildTreeFromPartials(Vector((0, "a", List(Tree("b")))), Nil) shouldBe List(Tree("a", Tree("b")))
      buildTreeFromPartials(Vector((0, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"))
      )
      buildTreeFromPartials(Vector((0, "d", Nil), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      )
      buildTreeFromPartials(Vector((0, "d", List(Tree("e", Tree("f")))), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f"))))
      )
      buildTreeFromPartials(
        Vector((0, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"))))
      buildTreeFromPartials(
        Vector((0, "h", Nil), (1, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"), Tree("h"))))
      buildTreeFromPartials(
        Vector((0, "h", Nil), (0, "d", List(Tree("e", Tree("f")), Tree("g"))), (2, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g")), Tree("h")))
      buildTreeFromPartials(
        Vector(
          (0, "h", List(Tree("i"), Tree("j"))),
          (0, "d", List(Tree("e", Tree("f")), Tree("g"))),
          (2, "a", List(Tree("b"), Tree("c")))
        ),
        Nil
      ) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g")), Tree("h", Tree("i"), Tree("j")))
      )
    }
  }
}
