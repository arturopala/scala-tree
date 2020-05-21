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

    "ensure child is distinct" in {
      ensureChildDistinct(Tree("a"), 0) shouldBe Tree("a")
      ensureChildDistinct(Tree("a"), 1) shouldBe Tree("a")
      ensureChildDistinct(Tree("a", Tree("b")), 0) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b")), 1) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 0) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 1) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 0) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 1) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 2) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b"), Tree("b")), 2) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("b", Tree("d"), Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 1) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("b", Tree("d"), Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 0) shouldBe Tree(
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
      buildTreeFromPartials(List(), Nil) shouldBe Nil
      buildTreeFromPartials(List((0, "a", Nil)), Nil) shouldBe List(Tree("a"))
      buildTreeFromPartials(List((0, "a", List(Tree("b")))), Nil) shouldBe List(Tree("a", Tree("b")))
      buildTreeFromPartials(List((0, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"))
      )
      buildTreeFromPartials(List((0, "d", Nil), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      )
      buildTreeFromPartials(List((0, "d", List(Tree("e", Tree("f")))), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f"))))
      )
      buildTreeFromPartials(
        List((0, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"))))
      buildTreeFromPartials(
        List((0, "h", Nil), (1, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"), Tree("h"))))
      buildTreeFromPartials(
        List((0, "h", Nil), (0, "d", List(Tree("e", Tree("f")), Tree("g"))), (2, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g")), Tree("h")))
      buildTreeFromPartials(
        List(
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
