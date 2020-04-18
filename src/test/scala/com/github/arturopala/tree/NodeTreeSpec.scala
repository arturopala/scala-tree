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

import com.github.arturopala.tree.TreeFormat._
import com.github.arturopala.tree.util.NodeTree
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeTreeSpec extends AnyWordSpec with Matchers {

  import TestTrees._

  def all[T]: T => Boolean = _ => true
  def none[T]: T => Boolean = _ => false
  val even: String => Boolean = s => s.head.toInt % 2 == 0
  val odd: String => Boolean = s => s.head.toInt  % 2 != 0

  s"NodeTree" should {

    "stream all values" in {
      NodeTree.valueStream(all, tree1).toList shouldBe List("a")
      NodeTree.valueStream(all, tree2).toList shouldBe List("a", "b")
      NodeTree.valueStream(all, tree3_1).toList shouldBe List("a", "b", "c")
      NodeTree.valueStream(all, tree3_2).toList shouldBe List("a", "b", "c")
      NodeTree.valueStream(all, tree4_1).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree4_2).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree4_3).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree7).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      NodeTree.valueStream(all, tree9).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "stream filtered nodes" in {
      NodeTree.valueStream(all, tree1).toList shouldBe List("a")
      NodeTree.valueStream(all, tree2).toList shouldBe List("a", "b")
      NodeTree.valueStream(all, tree3_1).toList shouldBe List("a", "b", "c")
      NodeTree.valueStream(all, tree3_2).toList shouldBe List("a", "b", "c")
      NodeTree.valueStream(all, tree4_1).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree4_2).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree4_3).toList shouldBe List("a", "b", "c", "d")
      NodeTree.valueStream(all, tree7).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      NodeTree.valueStream(all, tree9).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      NodeTree.valueStream(none, tree1).toList shouldBe Nil
      NodeTree.valueStream(none, tree2).toList shouldBe Nil
      NodeTree.valueStream(none, tree3_1).toList shouldBe Nil
      NodeTree.valueStream(none, tree3_2).toList shouldBe Nil
      NodeTree.valueStream(none, tree4_1).toList shouldBe Nil
      NodeTree.valueStream(none, tree4_2).toList shouldBe Nil
      NodeTree.valueStream(none, tree4_3).toList shouldBe Nil
      NodeTree.valueStream(none, tree7).toList shouldBe Nil
      NodeTree.valueStream(none, tree9).toList shouldBe Nil

      NodeTree.valueStream(even, tree1).toList shouldBe Nil
      NodeTree.valueStream(even, tree2).toList shouldBe List("b")
      NodeTree.valueStream(even, tree3_1).toList shouldBe List("b")
      NodeTree.valueStream(even, tree3_2).toList shouldBe List("b")
      NodeTree.valueStream(even, tree4_1).toList shouldBe List("b", "d")
      NodeTree.valueStream(even, tree4_2).toList shouldBe List("b", "d")
      NodeTree.valueStream(even, tree4_3).toList shouldBe List("b", "d")
      NodeTree.valueStream(even, tree7).toList shouldBe List("b", "d", "f")
      NodeTree.valueStream(even, tree9).toList shouldBe List("b", "d", "f", "h")

      NodeTree.valueStream(odd, tree1).toList shouldBe List("a")
      NodeTree.valueStream(odd, tree2).toList shouldBe List("a")
      NodeTree.valueStream(odd, tree3_1).toList shouldBe List("a", "c")
      NodeTree.valueStream(odd, tree3_2).toList shouldBe List("a", "c")
      NodeTree.valueStream(odd, tree4_1).toList shouldBe List("a", "c")
      NodeTree.valueStream(odd, tree4_2).toList shouldBe List("a", "c")
      NodeTree.valueStream(odd, tree4_3).toList shouldBe List("a", "c")
      NodeTree.valueStream(odd, tree7).toList shouldBe List("a", "c", "e", "g")
      NodeTree.valueStream(odd, tree9).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "stream all branches" in {
      NodeTree.branchStream(all, tree1) shouldBe List(List("a"))
      NodeTree.branchStream(all, tree2) shouldBe List(List("a", "b"))
      NodeTree.branchStream(all, tree3_1) shouldBe List(List("a", "b", "c"))
      NodeTree.branchStream(all, tree3_2) shouldBe List(List("a", "b"), List("a", "c"))
      NodeTree.branchStream(all, tree4_1) shouldBe List(List("a", "b", "c", "d"))
      NodeTree.branchStream(all, tree4_2) shouldBe List(List("a", "b", "c"), List("a", "d"))
      NodeTree.branchStream(all, tree4_3) shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      NodeTree.branchStream(all, tree7) shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      NodeTree.branchStream(all, tree9) shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
    }

    "stream filtered branches" in {
      NodeTree.branchStream(all, tree1).map(_.toList).toList shouldBe List(List("a"))
      NodeTree.branchStream(all, tree2).map(_.toList).toList shouldBe List(List("a", "b"))
      NodeTree.branchStream(all, tree3_1).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      NodeTree.branchStream(all, tree3_2).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      NodeTree.branchStream(all, tree4_1).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      NodeTree.branchStream(all, tree4_2).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      NodeTree.branchStream(all, tree4_3).map(_.toList).toList shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      NodeTree.branchStream(all, tree7).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      NodeTree.branchStream(all, tree9).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      NodeTree.branchStream(none, tree1).toList shouldBe Nil
      NodeTree.branchStream(none, tree2).toList shouldBe Nil
      NodeTree.branchStream(none, tree3_1).toList shouldBe Nil
      NodeTree.branchStream(none, tree3_2).toList shouldBe Nil
      NodeTree.branchStream(none, tree4_1).toList shouldBe Nil
      NodeTree.branchStream(none, tree4_2).toList shouldBe Nil
      NodeTree.branchStream(none, tree4_3).toList shouldBe Nil
      NodeTree.branchStream(none, tree7).toList shouldBe Nil
      NodeTree.branchStream(none, tree9).toList shouldBe Nil

      NodeTree.branchStream[String](_.size > 1, tree1).toList shouldBe Nil
      NodeTree.branchStream[String](_.size > 1, tree2).map(_.toList).toList shouldBe List(List("a", "b"))
      NodeTree.branchStream[String](_.last == "c", tree3_1).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      NodeTree.branchStream[String](_.last == "c", tree3_2).map(_.toList).toList shouldBe List(List("a", "c"))
      NodeTree.branchStream[String](_.last == "d", tree4_2).map(_.toList).toList shouldBe List(List("a", "d"))
      NodeTree.branchStream[String](_.size > 2, tree4_2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      NodeTree.branchStream[String](_.size > 3, tree7).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
      NodeTree.branchStream[String](_.size > 3, tree9).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      NodeTree.branchStream[String](_.toList.contains("e"), tree9).map(_.toList).toList shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      NodeTree.branchStream[String](_.toList.contains("h"), tree9).map(_.toList).toList shouldBe List(
        List("a", "e", "h", "i")
      )
      NodeTree.branchStream[String](_.size < 3, tree9).map(_.toList).toList shouldBe Nil
    }

    "stream all subtrees" in {
      NodeTree.treeStream(all, tree1).toList shouldBe List(tree1)
      NodeTree.treeStream(all, tree2).toList shouldBe List(tree2, Tree("b"))
      NodeTree.treeStream(all, tree3_1).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      NodeTree.treeStream(all, tree3_2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      NodeTree.treeStream(all, tree4_1).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      NodeTree.treeStream(all, tree4_2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      NodeTree.treeStream(all, tree4_3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      NodeTree.treeStream(all, tree7).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      NodeTree.treeStream(all, tree9).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin
    }

    "stream filtered subtrees" in {
      NodeTree.treeStream(all, tree1).toList shouldBe List(tree1)
      NodeTree.treeStream(all, tree2).toList shouldBe List(tree2, Tree("b"))
      NodeTree.treeStream(all, tree3_1).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      NodeTree.treeStream(all, tree3_2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      NodeTree.treeStream(all, tree4_1).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      NodeTree.treeStream(all, tree4_2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      NodeTree.treeStream(all, tree4_3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      NodeTree.treeStream(all, tree7).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      NodeTree.treeStream(all, tree9).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      NodeTree.treeStream(none, tree1).toList shouldBe Nil
      NodeTree.treeStream(none, tree2).toList shouldBe Nil
      NodeTree.treeStream(none, tree3_1).toList shouldBe Nil
      NodeTree.treeStream(none, tree3_2).toList shouldBe Nil
      NodeTree.treeStream(none, tree4_1).toList shouldBe Nil
      NodeTree.treeStream(none, tree4_2).toList shouldBe Nil
      NodeTree.treeStream(none, tree4_3).toList shouldBe Nil
      NodeTree.treeStream(none, tree7).toList shouldBe Nil
      NodeTree.treeStream(none, tree9).toList shouldBe Nil

      NodeTree.treeStream[String](_.size > 0, tree1).toList shouldBe List(tree1)
      NodeTree.treeStream[String](_.size > 0, tree2).toList shouldBe List(tree2, Tree("b"))
      NodeTree.treeStream[String](_.size < 2, tree3_2).toList shouldBe List(Tree("b"), Tree("c"))
      NodeTree.treeStream[String](_.height == 2, tree7).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))
      NodeTree.treeStream[String](_.height == 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      NodeTree.treeStream[String](_.height > 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      NodeTree.treeStream[String](_.width >= 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }
  }
}
