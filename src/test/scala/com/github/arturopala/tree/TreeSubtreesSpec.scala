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

class TreeSubtreesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "list all sub-trees" in {
      tree0.trees shouldBe Nil
      tree1.trees shouldBe List(tree1)
      tree2.trees shouldBe List(tree2, Tree("b"))
      tree3_1.trees shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.trees shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.trees shouldBe List(tree4_1, Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("d")), Tree("d"))
      tree4_2.trees shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.trees shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.trees.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.trees.map(_.showAsArrays()).mkString("\n") shouldBe
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

    "iterate over filtered subtrees" in {
      tree0.treeIterator(all).toList shouldBe Nil
      tree1.treeIterator(all).toList shouldBe List(tree1)
      tree2.treeIterator(all).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeIterator(all).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeIterator(all).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeIterator(all).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treeIterator(all).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeIterator(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeIterator(all).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treeIterator(none).toList shouldBe Nil
      tree1.treeIterator(none).toList shouldBe Nil
      tree2.treeIterator(none).toList shouldBe Nil
      tree3_1.treeIterator(none).toList shouldBe Nil
      tree3_2.treeIterator(none).toList shouldBe Nil
      tree4_1.treeIterator(none).toList shouldBe Nil
      tree4_2.treeIterator(none).toList shouldBe Nil
      tree4_3.treeIterator(none).toList shouldBe Nil
      tree7.treeIterator(none).toList shouldBe Nil
      tree9.treeIterator(none).toList shouldBe Nil

      tree0.treeIterator(_.size > 0).toList shouldBe Nil
      tree1.treeIterator(_.size > 0).toList shouldBe List(tree1)
      tree2.treeIterator(_.size > 0).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treeIterator(_.size < 2).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treeIterator(_.height == 2).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))
      tree9.treeIterator(_.height == 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treeIterator(_.height > 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treeIterator(_.width >= 2).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }

    "iterate over filtered subtrees with depth limit" in {
      tree0.treeIterator(all, 0).toList shouldBe Nil
      tree0.treeIterator(all, 1).toList shouldBe Nil
      tree1.treeIterator(all, 0).toList shouldBe Nil
      tree1.treeIterator(all, 1).toList shouldBe List(tree1)
      tree1.treeIterator(all, 2).toList shouldBe List(tree1)
      tree2.treeIterator(all, 0).toList shouldBe Nil
      tree2.treeIterator(all, 1).toList shouldBe List(tree2)
      tree2.treeIterator(all, 2).toList shouldBe List(tree2, Tree("b"))
      tree2.treeIterator(all, 3).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treeIterator(all, 0).toList shouldBe Nil
      tree3_1.treeIterator(all, 1).toList shouldBe List(tree3_1)
      tree3_1.treeIterator(all, 2).toList shouldBe List(tree3_1, Tree("b", Tree("c")))
      tree3_1.treeIterator(all, 3).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treeIterator(all, 0).toList shouldBe Nil
      tree3_2.treeIterator(all, 1).toList shouldBe List(tree3_2)
      tree3_2.treeIterator(all, 2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treeIterator(all, 0).toList shouldBe Nil
      tree4_1.treeIterator(all, 1).toList shouldBe List(tree4_1)
      tree4_1.treeIterator(all, 2).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_1.treeIterator(all, 3).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d"))
      )
      tree4_1.treeIterator(all, 4).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treeIterator(all, 0).toList shouldBe Nil
      tree4_2.treeIterator(all, 1).toList shouldBe List(tree4_2)
      tree4_2.treeIterator(all, 2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("d"))
      tree4_2.treeIterator(all, 3).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_2.treeIterator(all, 4).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all, 0).toList shouldBe Nil
      tree4_3.treeIterator(all, 1).toList shouldBe List(tree4_3)
      tree4_3.treeIterator(all, 2).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree4_3.treeIterator(all, 3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treeIterator(all, 0).toList shouldBe Nil
      tree7.treeIterator(all, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]""".stripMargin
      tree7.treeIterator(all, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]""".stripMargin
      tree7.treeIterator(all, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[g]""".stripMargin
      tree7.treeIterator(all, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treeIterator(all, 0).toList shouldBe Nil
      tree9.treeIterator(all, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]""".stripMargin
      tree9.treeIterator(all, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treeIterator(all, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treeIterator(all, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treeIterator(none, 10).toList shouldBe Nil
      tree1.treeIterator(none, 10).toList shouldBe Nil
      tree2.treeIterator(none, 10).toList shouldBe Nil
      tree3_1.treeIterator(none, 10).toList shouldBe Nil
      tree3_2.treeIterator(none, 10).toList shouldBe Nil
      tree4_1.treeIterator(none, 10).toList shouldBe Nil
      tree4_2.treeIterator(none, 10).toList shouldBe Nil
      tree4_3.treeIterator(none, 10).toList shouldBe Nil
      tree7.treeIterator(none, 10).toList shouldBe Nil
      tree9.treeIterator(none, 10).toList shouldBe Nil
    }

  }

}
