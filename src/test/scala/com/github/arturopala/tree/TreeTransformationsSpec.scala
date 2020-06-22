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

class TreeTransformationsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "map lax all nodes" in {
      val f: String => String = _ + "0"
      tree0.map(f).showAsGraph() shouldBe ""
      tree1.map(f).showAsGraph() shouldBe "a0"
      tree2.map(f).showAsGraph() shouldBe """a0 > b0"""
      tree3_1.map(f).showAsGraph() shouldBe """a0 > b0 > c0"""
      tree3_2.map(f).showAsGraph() shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin
      tree7.map(f).showAsGraph() shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }

    "flatMap lax all nodes" in {
      val f: String => Tree[String] = x => Tree(x, Tree(x + "0"))
      tree0.flatMapLax(f).showAsGraph() shouldBe ""
      tree1.flatMapLax(f).showAsGraph() shouldBe "a > a0"
      tree2.flatMapLax(f).showAsGraph() shouldBe
        """a > b > b0
          |a > a0""".stripMargin
      tree3_1.flatMapLax(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > a0""".stripMargin
      tree3_2.flatMapLax(f).showAsGraph() shouldBe
        """a > b > b0
          |a > c > c0
          |a > a0""".stripMargin
      tree7.flatMapLax(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > d > e > f > f0
          |a > d > e > e0
          |a > d > d0
          |a > g > g0
          |a > a0""".stripMargin

      val f2: String => Tree[String] = s => Tree(s, Tree(s + s), Tree(s + s + s))
      tree0.flatMapLax(f2) shouldBe tree0
      tree1.flatMapLax(f2) shouldBe Tree("a", Tree("aa"), Tree("aaa"))

      val f3: String => Tree[String] = s => Tree(s, Tree("b"))
      tree0.flatMapLax(f3) shouldBe tree0
      tree1.flatMapLax(f3) shouldBe Tree("a", Tree("b"))
      tree2.flatMapLax(f3) shouldBe Tree("a", Tree("b", Tree("b")), Tree("b"))
      tree3_1.flatMapLax(f3) shouldBe Tree("a", Tree("b", Tree("c", Tree("b")), Tree("b")), Tree("b"))
      tree3_2.flatMapLax(f3) shouldBe Tree("a", Tree("b", Tree("b")), Tree("c", Tree("b")), Tree("b"))

      val f4: String => Tree[String] = s => Tree("b", Tree(s))
      tree0.flatMapLax(f4) shouldBe tree0
      tree1.flatMapLax(f4) shouldBe Tree("b", Tree("a"))
      tree2.flatMapLax(f4) shouldBe Tree("b", Tree("b", Tree("b")), Tree("a"))
      tree3_1.flatMapLax(f4) shouldBe Tree("b", Tree("b", Tree("b", Tree("c")), Tree("b")), Tree("a"))
      tree3_2.flatMapLax(f4) shouldBe Tree("b", Tree("b", Tree("b")), Tree("b", Tree("c")), Tree("a"))
      tree4_1.flatMapLax(f4) shouldBe
        Tree("b", Tree("b", Tree("b", Tree("b", Tree("d")), Tree("c")), Tree("b")), Tree("a"))
      tree4_2.flatMapLax(f4) shouldBe
        Tree("b", Tree("b", Tree("b", Tree("c")), Tree("b")), Tree("b", Tree("d")), Tree("a"))
      tree4_3.flatMapLax(f4) shouldBe
        Tree("b", Tree("b", Tree("b")), Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("a"))
    }

    "flatMap distinct all nodes" in {
      val f2: String => Tree[String] = s => Tree(s, Tree(s + s), Tree(s + s + s))
      tree0.flatMap(f2) shouldBe tree0
      tree1.flatMap(f2) shouldBe Tree("a", Tree("aa"), Tree("aaa"))

      val f3: String => Tree[String] = s => Tree(s, Tree("b"))
      tree0.flatMap(f3) shouldBe tree0
      tree1.flatMap(f3) shouldBe Tree("a", Tree("b"))
      tree2.flatMap(f3) shouldBe Tree("a", Tree("b", Tree("b")))
      tree3_1.flatMap(f3) shouldBe Tree("a", Tree("b", Tree("c", Tree("b")), Tree("b")))
      tree3_2.flatMap(f3) shouldBe Tree("a", Tree("b", Tree("b")), Tree("c", Tree("b")))

      val f4: String => Tree[String] = s => Tree("b", Tree(s))
      tree0.flatMap(f4) shouldBe tree0
      tree1.flatMap(f4) shouldBe Tree("b", Tree("a"))
      tree2.flatMap(f4) shouldBe Tree("b", Tree("b", Tree("b")), Tree("a"))
      tree3_1.flatMap(f4) shouldBe Tree("b", Tree("b", Tree("b", Tree("c"))), Tree("a"))
      tree3_2.flatMap(f4) shouldBe Tree("b", Tree("b", Tree("b"), Tree("c")), Tree("a"))
      tree4_1.flatMap(f4) shouldBe
        Tree("b", Tree("b", Tree("b", Tree("b", Tree("d")), Tree("c"))), Tree("a"))
      tree4_2.flatMap(f4) shouldBe
        Tree("b", Tree("b", Tree("b", Tree("c")), Tree("d")), Tree("a"))
      tree4_3.flatMap(f4) shouldBe
        Tree("b", Tree("b", Tree("b"), Tree("c"), Tree("d")), Tree("a"))

    }
  }

}
