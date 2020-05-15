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

import com.github.arturopala.tree.internal.ArrayTree._
import com.github.arturopala.bufferandslice.{IntSlice, Slice}

import com.github.arturopala.tree.LaxTreeOps._

class ArrayTreeSpec extends AnyWordSpecCompat {

  val aa_a = Slice("aa", "a")
  val aaa_aa_a = Slice("aaa", "aa", "a")
  val a_aaa_aa_a = Slice("a", "aaa", "aa", "a")
  val aa_aaa_aa_a = Slice("aa", "aaa", "aa", "a")
  val aa_a_aaa_aa_a = Slice("aa", "a", "aaa", "aa", "a")

  final val id: String => String = x => x

  "ArrayTree" should {

    "iterate over tree's values with depth limit" in {
      val all: String => Boolean = _ => true
      val none: String => Boolean = _ => false
      valueIterator(0, Array(0), Array("a"), all, 0).toList shouldBe Nil
      valueIterator(0, Array(0), Array("a"), all, 1).toList shouldBe List("a")
      valueIterator(0, Array(0), Array("a"), none, 1).toList shouldBe Nil
      valueIterator(1, Array(0, 1), Array("b", "a"), all, 0).toList shouldBe Nil
      valueIterator(1, Array(0, 1), Array("b", "a"), all, 1).toList shouldBe List("a")
      valueIterator(1, Array(0, 1), Array("b", "a"), all, 2).toList shouldBe List("a", "b")
      valueIterator(2, Array(0, 1, 1), Array("c", "b", "a"), all, 0).toList shouldBe Nil
      valueIterator(2, Array(0, 1, 1), Array("c", "b", "a"), all, 1).toList shouldBe List("a")
      valueIterator(2, Array(0, 1, 1), Array("c", "b", "a"), all, 2).toList shouldBe List("a", "b")
      valueIterator(2, Array(0, 1, 1), Array("c", "b", "a"), all, 3).toList shouldBe List("a", "b", "c")
      valueIterator(2, Array(0, 1, 1), Array("c", "b", "a"), none, 3).toList shouldBe Nil
      valueIterator(2, Array(0, 0, 2), Array("c", "b", "a"), all, 0).toList shouldBe Nil
      valueIterator(2, Array(0, 0, 2), Array("c", "b", "a"), all, 1).toList shouldBe List("a")
      valueIterator(2, Array(0, 0, 2), Array("c", "b", "a"), all, 2).toList shouldBe List("a", "b", "c")
      valueIterator(2, Array(0, 0, 2), Array("c", "b", "a"), none, 2).toList shouldBe Nil
      valueIterator(3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), all, 0).toList shouldBe Nil
      valueIterator(3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), all, 1).toList shouldBe List("a")
      valueIterator(3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), all, 2).toList shouldBe List("a", "b", "d")
      valueIterator(3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), all, 3).toList shouldBe List("a", "b", "c", "d")
      valueIterator(3, Array(0, 1, 0, 2), Array("d", "c", "b", "a"), all, 0).toList shouldBe Nil
      valueIterator(3, Array(0, 1, 0, 2), Array("d", "c", "b", "a"), all, 1).toList shouldBe List("a")
      valueIterator(3, Array(0, 1, 0, 2), Array("d", "c", "b", "a"), all, 2).toList shouldBe List("a", "b", "c")
      valueIterator(3, Array(0, 1, 0, 2), Array("d", "c", "b", "a"), all, 3).toList shouldBe List("a", "b", "c", "d")
    }

    "iterate over tree's branches as values lists without filter" in {
      val v: Int => Int = _ * 10
      val f: Iterable[Int] => Boolean = _ => true
      branchIterator(0, Array(0), v, f, 0).map(_.toList).toList shouldBe Nil
      branchIterator(0, Array(0), v, f, 10).map(_.toList).toList shouldBe List(List(0))
      branchIterator(1, Array(0, 1), v, f, 10).map(_.toList).toList shouldBe List(List(10, 0))
      branchIterator(2, Array(0, 1, 1), v, f, 10).map(_.toList).toList shouldBe List(List(20, 10, 0))
      branchIterator(2, Array(0, 0, 2), v, f, 10).map(_.toList).toList shouldBe List(List(20, 10), List(20, 0))
      branchIterator(3, Array(0, 0, 0, 3), v, f, 10).map(_.toList).toList shouldBe List(
        List(30, 20),
        List(30, 10),
        List(30, 0)
      )
      branchIterator(3, Array(0, 0, 2, 1), v, f, 10).map(_.toList).toList shouldBe List(
        List(30, 20, 10),
        List(30, 20, 0)
      )
      branchIterator(3, Array(0, 0, 2, 1), v, f, 0).map(_.toList).toList shouldBe Nil
      branchIterator(3, Array(0, 0, 2, 1), v, f, 1).map(_.toList).toList shouldBe List(List(30))
      branchIterator(3, Array(0, 0, 2, 1), v, f, 2).map(_.toList).toList shouldBe List(List(30, 20))
      branchIterator(3, Array(0, 0, 2, 1), v, f, 3).map(_.toList).toList shouldBe List(
        List(30, 20, 10),
        List(30, 20, 0)
      )
    }

    "count branches" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _ => true
      countBranches[String](-1, Array.empty[Int], Array.empty[String], f) shouldBe 0
      countBranches[String](0, Array(0), v, f) shouldBe 1
      countBranches[String](0, Array(0, 1), v, f) shouldBe 1
      countBranches[String](1, Array(0, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 1, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2), v, f) shouldBe 2
      countBranches[String](3, Array(0, 0, 0, 3), v, f) shouldBe 3
      countBranches[String](3, Array(0, 0, 1, 2), v, f) shouldBe 2
      countBranches[String](2, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 2
      countBranches[String](7, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 4
      countBranches[String](9, Array(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), v, f) shouldBe 5
    }

    "count branches fulfilling the predicate" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _.size > 2
      countBranches[String](-1, Array.empty[Int], Array.empty[String], f) shouldBe 0
      countBranches[String](0, Array(0), v, f) shouldBe 0
      countBranches[String](0, Array(0, 1), v, f) shouldBe 0
      countBranches[String](1, Array(0, 1), v, f) shouldBe 0
      countBranches[String](2, Array(0, 1, 1), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2), v, f) shouldBe 0
      countBranches[String](3, Array(0, 0, 0, 3), v, f) shouldBe 0
      countBranches[String](3, Array(0, 0, 1, 2), v, f) shouldBe 1
      countBranches[String](2, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 0
      countBranches[String](7, Array(0, 0, 2, 0, 0, 1, 2, 2), v, f) shouldBe 4
      countBranches[String](9, Array(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), v, f) shouldBe 4
    }

    "access a tree at the index" in {
      treeAt(0, IntSlice(0), Slice("a")).height shouldBe 1
      treeAt(1, IntSlice(0, 1), Slice("a", "b")).height shouldBe 2
      treeAt(0, IntSlice(0, 1), Slice("a", "b")).height shouldBe 1
      treeAt(2, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 3
      treeAt(1, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 2
      treeAt(0, IntSlice(0, 1, 1), Slice("a", "b", "c")).height shouldBe 1
    }

    "iterate over tree's subtrees" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treeIterator(0, IntSlice(0), Slice("a"), f).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treeIterator(1, IntSlice(0, 1), Slice("b", "a"), f).toList shouldBe List(Tree("a", Tree("b")), Tree("b"))
      treeIterator(2, IntSlice(0, 1, 1), Slice("c", "b", "a"), f).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c")),
        Tree("c")
      )
      treeIterator(1, IntSlice(0, 1, 1), Slice("c", "b", "a"), f).toList shouldBe List(Tree("b", Tree("c")), Tree("c"))
      treeIterator(2, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treeIterator(1, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(Tree("b"))
      treeIterator(0, IntSlice(0, 0, 2), Slice("c", "b", "a"), f).toList shouldBe List(Tree("c"))
      treeIterator(2, IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a"), f).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "select a value by the path" in {
      selectValue(List("a"), -1, IntSlice(), Slice.empty[String], id) shouldBe None
      selectValue(List("a"), 0, Array(0), Array("a"), id) shouldBe Some("a")
      selectValue(List("a", "b"), 0, Array(0), Array("a"), id) shouldBe None
      selectValue(List("a", "a"), 0, Array(0), Array("a"), id) shouldBe None
      selectValue(List("a", "b"), 0, Array(0, 1), Array("b", "a"), id) shouldBe None
      selectValue(List("a", "b"), 1, Array(0, 1), Array("b", "a"), id) shouldBe Some("b")
      selectValue(List("a", "b"), 1, Array(0, 1), Array("c", "a"), id) shouldBe None
      selectValue(List("a", "b"), 2, Array(0, 0, 2), Array("c", "b", "a"), id) shouldBe Some("b")
      selectValue(List("a", "c"), 2, Array(0, 0, 2), Array("c", "b", "a"), id) shouldBe Some("c")
      selectValue(List("a", "d"), 3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), id) shouldBe Some("d")
      selectValue(List("a", "b", "c"), 3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), id) shouldBe Some("c")
      selectValue(List("a", "b", "c"), 3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), id) shouldBe Some("c")
      selectValue(List("a", "b"), 3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), id) shouldBe Some("b")
      selectValue(List("a", "c"), 3, Array(0, 0, 1, 2), Array("d", "c", "b", "a"), id) shouldBe None
    }

    "select a tree by the path" in {
      selectTree(List("a"), -1, IntSlice(), Slice.empty[String]) shouldBe None
      selectTree(List("a"), 0, IntSlice(0), Slice("a")) shouldBe Some(Tree("a"))
      selectTree(List("a", "b"), 0, IntSlice(0), Slice("a")) shouldBe None
      selectTree(List("a", "a"), 0, IntSlice(0), Slice("a")) shouldBe None
      selectTree(List("a", "b"), 0, IntSlice(0, 1), Slice("b", "a")) shouldBe None
      selectTree(List("a", "b"), 1, IntSlice(0, 1), Slice("b", "a")) shouldBe Some(Tree("b"))
      selectTree(List("a", "b"), 1, IntSlice(0, 1), Slice("c", "a")) shouldBe None
      selectTree(List("a", "b"), 2, IntSlice(0, 0, 2), Slice("c", "b", "a")) shouldBe Some(Tree("b"))
      selectTree(List("a", "c"), 2, IntSlice(0, 0, 2), Slice("c", "b", "a")) shouldBe Some(Tree("c"))
      selectTree(List("a", "d"), 3, IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")) shouldBe Some(Tree("d"))
      selectTree(List("a", "b", "c"), 3, IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")) shouldBe Some(Tree("c"))
      selectTree(List("a", "b", "c"), 3, IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")) shouldBe Some(Tree("c"))
      selectTree(List("a", "b"), 3, IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")) shouldBe Some(Tree("b", Tree("c")))
      selectTree(List("a", "c"), 3, IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")) shouldBe None
    }

    "select a tree by the path using extractor function" in {
      val length: String => Int = (s: String) => s.length
      selectTree(List(1), -1, IntSlice(), Slice.empty[String], length) shouldBe None
      selectTree(List(1), 0, IntSlice(0), Slice("a"), length) shouldBe Some(Tree("a"))
      selectTree(List(0), 0, IntSlice(0), Slice("a"), length) shouldBe None
      selectTree(List(1), 1, IntSlice(0, 1), Slice("b", "a"), length) shouldBe Some(Tree("a", Tree("b")))
      selectTree(List(1, 1), 1, IntSlice(0, 1), Slice("b", "a"), length) shouldBe Some(Tree("b"))
      selectTree(List(1, 0), 1, IntSlice(0, 1), Slice("b", "a"), length) shouldBe None
      selectTree(List(0, 1), 1, IntSlice(0, 1), Slice("b", "a"), length) shouldBe None
      selectTree(List(1, 1), 2, IntSlice(0, 0, 2), Slice("c", "b", "a"), length) shouldBe Some(Tree("c"))
      selectTree(List(1, 2, 3), 4, IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a"), length) shouldBe Some(
        Tree("aaa")
      )
      selectTree(List(1, 4), 4, IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a"), length) shouldBe Some(
        Tree("aaaa", Tree("aaaaa"))
      )
      selectTree(List(1, 2), 4, IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a"), length) shouldBe Some(
        Tree("aa", Tree("aaa"))
      )
    }

    "flatMap a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMap(IntSlice(), Slice.empty[String], f0) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f0) shouldBe Tree(1)
      flatMap(IntSlice(0, 1), aa_a, f0) shouldBe Tree(1, Tree(2))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMap(IntSlice(0, 0, 2), aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMap(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMap(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMap(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMap(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMap(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMap(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1, Tree(2))))
      flatMap(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMap(IntSlice(), Slice.empty[String], f1) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f1) shouldBe Tree(1, Tree(2))
      flatMap(IntSlice(0, 1), aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(2))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(2)
      )
      flatMap(IntSlice(0, 0, 2), aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(2)
      )

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMap(IntSlice(), Slice.empty[String], f2) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f2) shouldBe Tree(1, Tree(2), Tree(2))
      flatMap(IntSlice(0, 1), aa_a, f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)), Tree(2), Tree(2))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4), Tree(3)),
        Tree(2),
        Tree(2)
      )

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMap(IntSlice(), Slice.empty[String], f3) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f3) shouldBe Tree.empty
      flatMap(IntSlice(0, 1), aa_a, f3) shouldBe Tree.empty
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMap(IntSlice(), Slice.empty[String], f4) shouldBe Tree.empty
      flatMap(IntSlice(0), Slice("a"), f4) shouldBe Tree.empty
      flatMap(IntSlice(0, 1), aa_a, f4) shouldBe Tree(2, Tree(4))
      flatMap(IntSlice(0, 1, 1), aaa_aa_a, f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))

      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0), Slice("a"), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0, 1), Slice("b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMap(IntSlice(0, 0, 2), Slice("c", "b", "a"), f5) shouldBe
        Tree(
          0,
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(1, Tree(2, Tree(4, Tree(5))))
        )
    }

    "flatMap distinct a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMapDistinct(IntSlice(), Slice.empty[String], f0) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f0) shouldBe Tree(1)
      flatMapDistinct(IntSlice(0, 1), aa_a, f0) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1, Tree(2))))
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(IntSlice(), Slice.empty[String], f1) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f1) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1), aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f1) shouldBe Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(3, Tree(6)))
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1, Tree(2)), Tree(6)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(1, Tree(2)), Tree(6))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2, Tree(4))), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f1) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)), Tree(1, Tree(2, Tree(4))))

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMapDistinct(IntSlice(), Slice.empty[String], f2) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f2) shouldBe Tree(1, Tree(2))
      flatMapDistinct(IntSlice(0, 1), aa_a, f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f2) shouldBe Tree(1, Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(6), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 0, 3), aa_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(3, Tree(6), Tree(4)),
        Tree(2, Tree(4), Tree(3))
      )
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(1, Tree(2)), Tree(6), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))), Tree(4))
      )
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f2) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))))

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMapDistinct(IntSlice(), Slice.empty[String], f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1), aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 1, 1), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 0, 3), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 2, 1), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 0, 1, 2), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 2), a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a, f3) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a, f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(IntSlice(), Slice.empty[String], f4) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0), Slice("a"), f4) shouldBe Tree.empty
      flatMapDistinct(IntSlice(0, 1), aa_a, f4) shouldBe Tree(2, Tree(4))
      flatMapDistinct(IntSlice(0, 1, 1), aaa_aa_a, f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))
      flatMapDistinct(IntSlice(0, 0, 2), aaa_aa_a, f4) shouldBe Tree.empty

      // test direct subtree insertion
      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0), Slice("a"), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0, 1), Slice("b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(IntSlice(0, 0, 2), Slice("c", "b", "a"), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
    }

    "insert a value" in {
      insertValue(0, "a", Tree("a"), false) shouldBe Tree("a", Tree("a"))
      insertValue(1, "a", Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertValue(0, "a", Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("b", Tree("a")))
      insertValue(2, "a", Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      insertValue(1, "a", Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      insertValue(0, "a", Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      insertValue(0, "a", Tree("a", Tree("b", Tree("c"))), false) shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      insertValue(1, "a", Tree("a", Tree("b", Tree("c"))), false) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      insertValue(2, "a", Tree("a", Tree("b", Tree("c"))), false) shouldBe Tree("a", Tree("a"), Tree("b", Tree("c")))
    }

    "insert a subtree" in {
      //insertSubtree(0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertTree(0, Tree("a"), Tree.empty) shouldBe Tree("a")
      insertTree(0, Tree.empty, Tree("a")) shouldBe Tree("a")
      insertTree(0, Tree("a"), Tree("a")) shouldBe Tree("a", Tree("a"))
      insertTree(1, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertTree(0, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertTree(1, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")), Tree("b"))
      insertTree(0, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("b", Tree("a"))))
      insertTree(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b")))) shouldBe Tree(
        "b",
        Tree("a", Tree("b")),
        Tree("a", Tree("b"))
      )
    }

    "insert a subtree distinct" in {
      //insertSubtreeDistinct(0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertTreeDistinct(0, Tree("a"), Tree.empty) shouldBe Tree("a")
      insertTreeDistinct(0, Tree.empty, Tree("a")) shouldBe Tree("a")
      insertTreeDistinct(0, Tree("a"), Tree("a")) shouldBe Tree("a", Tree("a"))
      insertTreeDistinct(1, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertTreeDistinct(0, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertTreeDistinct(1, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertTreeDistinct(0, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree(
        "a",
        Tree("b", Tree("b", Tree("a")))
      )
      insertTreeDistinct(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b")))) shouldBe Tree(
        "b",
        Tree("a", Tree("b"))
      )
      insertTreeDistinct(2, Tree("b", Tree("d"), Tree("e")), Tree("a", Tree("b"), Tree("c"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
    }

    "insert a branch into the tree" in {
      //insertBranch(-1, List(), Tree.empty) shouldBe Tree.empty
      insertBranch(-1, List("a"), Tree.empty) shouldBe Tree("a")
      insertBranch(-1, List("a", "b", "c", "d", "e"), Tree.empty) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d", Tree("e"))))
      )
      insertBranch(0, List("a"), Tree("a")) shouldBe Tree("a")
      insertBranch(1, List("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "b"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "c"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("c"), Tree("b"))
      insertBranch(3, List("a", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "c", "e"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("e")),
        Tree("d")
      )
      insertBranch(3, List("a", "b", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "d", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d", Tree("c"))
      )
      insertBranch(3, List("a", "e", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d"))) shouldBe Tree(
        "a",
        Tree("e", Tree("c")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
    }

    "update value at the index" in {
      updateValue(0, None, "c", Tree("a").deflated, false) shouldBe Tree("c")
      updateValue(0, None, "c", Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("c"))
      updateValue(1, None, "c", Tree("a", Tree("b")).deflated, false) shouldBe Tree("c", Tree("b"))
      updateValue(0, None, "c", Tree("a", Tree("b"), Tree("d")).deflated, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c")
      )
      updateValue(1, None, "c", Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("c"),
        Tree("c")
      )
      updateValue(2, None, "c", Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "c",
        Tree("b"),
        Tree("c")
      )

    }

    "update distinct value at the index" in {
      updateValue(0, None, "c", Tree("a").deflated, true) shouldBe Tree("c")
      updateValue(0, None, "c", Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("c"))
      updateValue(1, None, "c", Tree("a", Tree("b")).deflated, true) shouldBe Tree("c", Tree("b"))
      updateValue(0, None, "c", Tree("a", Tree("b"), Tree("d")).deflated, true) shouldBe Tree("a", Tree("b"), Tree("c"))
      updateValue(1, None, "c", Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("c"))
      updateValue(2, None, "c", Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("c", Tree("b"), Tree("c"))
    }

    "modify value at the index" in {
      val f: String => String = (_: String) => "c"
      modifyValue(0, None, f, Tree("a").deflated, false) shouldBe Tree("c")
      modifyValue(0, None, f, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("c"))
      modifyValue(1, None, f, Tree("a", Tree("b")).deflated, false) shouldBe Tree("c", Tree("b"))
      modifyValue(0, None, f, Tree("a", Tree("b"), Tree("d")).deflated, false) shouldBe Tree("a", Tree("b"), Tree("c"))
      modifyValue(1, None, f, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("a", Tree("c"), Tree("c"))
      modifyValue(2, None, f, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("c", Tree("b"), Tree("c"))
    }

    "modify distinct value at the index" in {
      val f: String => String = (_: String) => "c"
      modifyValue(0, None, f, Tree("a").deflated, true) shouldBe Tree("c")
      modifyValue(0, None, f, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("c"))
      modifyValue(1, None, f, Tree("a", Tree("b")).deflated, true) shouldBe Tree("c", Tree("b"))
      modifyValue(0, None, f, Tree("a", Tree("b"), Tree("d")).deflated, true) shouldBe Tree("a", Tree("b"), Tree("c"))
      modifyValue(1, None, f, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("c"))
      modifyValue(2, None, f, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("c", Tree("b"), Tree("c"))
    }

    "remove value at index" in {
      removeValue(0, None, Tree("a").deflated, false) shouldBe Tree.empty
      removeValue(-1, None, Tree("a").deflated, false) shouldBe Tree("a")
      removeValue(1, None, Tree("a").deflated, false) shouldBe Tree("a")
      removeValue(1, None, Tree("a", Tree("b")).deflated, false) shouldBe Tree("b")
      removeValue(0, None, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a")
      removeValue(0, None, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("a", Tree("b"))
      removeValue(1, None, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("a", Tree("c"))
      a[RuntimeException] shouldBe thrownBy {
        removeValue(2, None, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("b")
      }
      removeValue(4, None, Tree("a", Tree("b", Tree("c")), Tree("c", Tree("e", Tree("f")))).deflated, false) shouldBe Tree(
        "a",
        Tree("c"),
        Tree("c", Tree("e", Tree("f")))
      )
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e", Tree("f")))).deflated,
        false
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g"))), Tree("c", Tree("e", Tree("f"))))
    }

    "remove distinct value at index" in {
      removeValue(0, None, Tree("a").deflated, true) shouldBe Tree.empty
      removeValue(-1, None, Tree("a").deflated, true) shouldBe Tree("a")
      removeValue(1, None, Tree("a").deflated, true) shouldBe Tree("a")
      removeValue(1, None, Tree("a", Tree("b")).deflated, true) shouldBe Tree("b")
      removeValue(0, None, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a")
      removeValue(0, None, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("b"))
      removeValue(1, None, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("c"))
      a[RuntimeException] shouldBe thrownBy {
        removeValue(2, None, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("b")
      }
      removeValue(4, None, Tree("a", Tree("b", Tree("c")), Tree("c", Tree("e", Tree("f")))).deflated, true) shouldBe Tree(
        "a",
        Tree("c", Tree("e", Tree("f")))
      )
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e", Tree("f")))).deflated,
        true
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g"), Tree("f"))))
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e"), Tree("f"))).deflated,
        true
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g")), Tree("f")))
    }

    "update the tree at the index" in {
      updateTree(0, None, Tree("b"), Tree("a"), false) shouldBe Tree("b")
      updateTree(0, None, Tree.empty, Tree("c"), false) shouldBe Tree.empty
      updateTree(0, None, Tree.empty, Tree("a", Tree("b")), false) shouldBe Tree("a")
      updateTree(1, None, Tree.empty, Tree("a", Tree("b")), false) shouldBe Tree.empty
      updateTree(0, None, Tree("c"), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("c"))
      updateTree(1, None, Tree("c"), Tree("a", Tree("b")), false) shouldBe Tree("c")
      updateTree(0, None, Tree("b", Tree("a")), Tree("c"), false) shouldBe Tree("b", Tree("a"))
      updateTree(0, None, Tree("a", Tree("b")), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("a", Tree("b")))
      updateTree(1, None, Tree("a", Tree("b")), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("b"))
      updateTree(1, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("a", Tree("b")),
        Tree("c")
      )
      updateTree(0, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("a", Tree("b"))
      )
      updateTree(2, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b"))
      updateTree(0, None, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("d"))
      )
      updateTree(0, None, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("b", Tree("d"))
      )
      updateTree(
        1,
        None,
        Tree("b", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("b", Tree("d"), Tree("e")), Tree("b"))
      updateTree(
        4,
        None,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("c"), Tree("b"))
      updateTree(
        5,
        None,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("c", Tree("d"), Tree("e"))
    }

    "update distinct the tree at the index" in {
      updateTree(0, None, Tree("b"), Tree("a"), true) shouldBe Tree("b")
      updateTree(0, None, Tree.empty, Tree("c"), true) shouldBe Tree.empty
      updateTree(0, None, Tree.empty, Tree("a", Tree("b")), true) shouldBe Tree("a")
      updateTree(1, None, Tree.empty, Tree("a", Tree("b")), true) shouldBe Tree.empty
      updateTree(0, None, Tree("c"), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("c"))
      updateTree(1, None, Tree("c"), Tree("a", Tree("b")), true) shouldBe Tree("c")
      updateTree(0, None, Tree("b", Tree("a")), Tree("c"), true) shouldBe Tree("b", Tree("a"))
      updateTree(0, None, Tree("a", Tree("b")), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("a", Tree("b")))
      updateTree(1, None, Tree("a", Tree("b")), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("b"))
      updateTree(1, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("a", Tree("b")),
        Tree("c")
      )
      updateTree(0, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("a", Tree("b"))
      )
      updateTree(2, None, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("b"))
      updateTree(0, None, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d"))
      )
      updateTree(0, None, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("c")
      )
      updateTree(1, None, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
      updateTree(1, None, Tree("b", Tree("d")), Tree("a", Tree("b", Tree("d")), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
      updateTree(
        1,
        None,
        Tree("b", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("a", Tree("b", Tree("e"), Tree("d"), Tree("f")), Tree("b"))
      updateTree(
        4,
        None,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("b"))
      updateTree(
        5,
        None,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("c", Tree("d"), Tree("e"))
      updateTree(
        5,
        None,
        Tree("c", Tree("d"), Tree("d")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("c", Tree("d"), Tree("d"))
    }

    "modify the tree at the index" in {
      val t2 = Tree("v", Tree("x"), Tree("y", Tree("z")))
      val f1: Tree[String] => Tree[String] = _.insertValueLax("z")
      val f2: Tree[String] => Tree[String] = _.insertTreeLax(t2)
      val f3: Tree[String] => Tree[String] = t => t.children.headOption.getOrElse(Tree("z")).insertValueLax(t.value)
      val f4: Tree[String] => Tree[String] = t => Tree.empty

      modifyTree(0, None, f1, Tree("a").deflated, false) shouldBe Tree("a", Tree("z"))
      modifyTree(0, None, f1, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("b", Tree("z")))
      modifyTree(1, None, f1, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("z"), Tree("b"))
      modifyTree(2, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("z"),
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b", Tree("z")),
        Tree("c")
      )
      modifyTree(0, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("z"))
      )

      modifyTree(0, None, f2, Tree("a").deflated, false) shouldBe Tree("a", t2)
      modifyTree(0, None, f2, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("b", t2))
      modifyTree(1, None, f2, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", t2, Tree("b"))
      modifyTree(2, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        t2,
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b", t2),
        Tree("c")
      )
      modifyTree(0, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", t2)
      )

      modifyTree(0, None, f3, Tree("a").deflated, false) shouldBe Tree("z", Tree("a"))
      modifyTree(0, None, f3, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("z", Tree("b")))
      modifyTree(1, None, f3, Tree("a", Tree("b")).deflated, false) shouldBe Tree("b", Tree("a"))
      modifyTree(2, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("b", Tree("a"))
      modifyTree(1, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("z", Tree("b")),
        Tree("c")
      )
      modifyTree(0, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("z", Tree("c"))
      )

      modifyTree(0, None, f4, Tree("a").deflated, false) shouldBe Tree.empty
      modifyTree(0, None, f4, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a")
      modifyTree(1, None, f4, Tree("a", Tree("b")).deflated, false) shouldBe Tree.empty
      modifyTree(2, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree.empty
      modifyTree(1, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("a", Tree("c"))
      modifyTree(0, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree("a", Tree("b"))

      def previous(s: String): String = s match {
        case "b" => "a"
        case "c" => "b"
        case "d" => "c"
        case _   => "z"
      }
      val f5: Tree[String] => Tree[String] = t => Tree(previous(t.value), Tree("x"), Tree("y"))

      modifyTree(0, None, f5, Tree("a").deflated, false) shouldBe Tree("z", Tree("x"), Tree("y"))
      modifyTree(0, None, f5, Tree("a", Tree("b")).deflated, false) shouldBe Tree("a", Tree("a", Tree("x"), Tree("y")))
      modifyTree(0, None, f5, Tree("a", Tree("b"), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("x"), Tree("y"))
      )
      modifyTree(0, None, f5, Tree("a", Tree("b", Tree("x"), Tree("z")), Tree("c")).deflated, false) shouldBe Tree(
        "a",
        Tree("b", Tree("x"), Tree("z")),
        Tree("b", Tree("x"), Tree("y"))
      )
    }

    "modify distinct the tree at the index" in {
      val t2 = Tree("v", Tree("x"), Tree("y", Tree("z")))
      val f1: Tree[String] => Tree[String] = _.insertValueLax("z")
      val f2: Tree[String] => Tree[String] = _.insertTreeLax(t2)
      val f3: Tree[String] => Tree[String] = t => t.children.headOption.getOrElse(Tree("z")).insertValueLax(t.value)
      val f4: Tree[String] => Tree[String] = t => Tree.empty

      modifyTree(0, None, f1, Tree("a").deflated, true) shouldBe Tree("a", Tree("z"))
      modifyTree(0, None, f1, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("b", Tree("z")))
      modifyTree(1, None, f1, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("z"), Tree("b"))
      modifyTree(2, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("z"),
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b", Tree("z")),
        Tree("c")
      )
      modifyTree(0, None, f1, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("z"))
      )

      modifyTree(0, None, f2, Tree("a").deflated, true) shouldBe Tree("a", t2)
      modifyTree(0, None, f2, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("b", t2))
      modifyTree(1, None, f2, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", t2, Tree("b"))
      modifyTree(2, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        t2,
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b", t2),
        Tree("c")
      )
      modifyTree(0, None, f2, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", t2)
      )

      modifyTree(0, None, f3, Tree("a").deflated, true) shouldBe Tree("z", Tree("a"))
      modifyTree(0, None, f3, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("z", Tree("b")))
      modifyTree(1, None, f3, Tree("a", Tree("b")).deflated, true) shouldBe Tree("b", Tree("a"))
      modifyTree(2, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("b", Tree("a"))
      modifyTree(1, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("z", Tree("b")),
        Tree("c")
      )
      modifyTree(0, None, f3, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("z", Tree("c"))
      )

      modifyTree(0, None, f4, Tree("a").deflated, true) shouldBe Tree.empty
      modifyTree(0, None, f4, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a")
      modifyTree(1, None, f4, Tree("a", Tree("b")).deflated, true) shouldBe Tree.empty
      modifyTree(2, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree.empty
      modifyTree(1, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("c"))
      modifyTree(0, None, f4, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree("a", Tree("b"))

      def previous(s: String): String = s match {
        case "b" => "a"
        case "c" => "b"
        case "d" => "c"
        case _   => "z"
      }
      val f5: Tree[String] => Tree[String] = t => Tree(previous(t.value), Tree("x"), Tree("y"))

      modifyTree(0, None, f5, Tree("a").deflated, true) shouldBe Tree("z", Tree("x"), Tree("y"))
      modifyTree(0, None, f5, Tree("a", Tree("b")).deflated, true) shouldBe Tree("a", Tree("a", Tree("x"), Tree("y")))
      modifyTree(0, None, f5, Tree("a", Tree("b"), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b", Tree("x"), Tree("y"))
      )
      modifyTree(0, None, f5, Tree("a", Tree("b", Tree("x"), Tree("z")), Tree("c")).deflated, true) shouldBe Tree(
        "a",
        Tree("b", Tree("y"), Tree("x"), Tree("z"))
      )
    }

    "remove tree at the index" in {
      removeTree(0, None, Tree("a")) shouldBe Tree.empty
      removeTree(-1, None, Tree("a")) shouldBe Tree("a")
      removeTree(1, None, Tree("a")) shouldBe Tree("a")
      removeTree(1, None, Tree("a", Tree("b"))) shouldBe Tree.empty
      removeTree(0, None, Tree("a", Tree("b"))) shouldBe Tree("a")
      removeTree(0, None, Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("b"))
      removeTree(1, None, Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("c"))
      removeTree(2, None, Tree("a", Tree("b"), Tree("c"))) shouldBe Tree.empty
      removeTree(0, None, Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b"))
      removeTree(1, None, Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a")
      removeTree(2, None, Tree("a", Tree("b", Tree("c")))) shouldBe Tree.empty
      removeTree(0, None, Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d"))
      )
      removeTree(1, None, Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("e")
      )
      removeTree(2, None, Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("e")
      )
      removeTree(3, None, Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("e"))) shouldBe Tree("a", Tree("e"))
      removeTree(4, None, Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("e"))) shouldBe Tree.empty
    }
  }

}
