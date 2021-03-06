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
import com.github.arturopala.bufferandslice.{IntSlice, RangeMapSlice, Slice}
import com.github.arturopala.tree.LaxTreeOps._
import com.github.arturopala.tree.internal.Transformer

import scala.reflect.ClassTag

class ArrayTreeSpec extends AnyWordSpecCompat {

  val aa_a = Slice("aa", "a")
  val aaa_aa_a = Slice("aaa", "aa", "a")
  val a_aaa_aa_a = Slice("a", "aaa", "aa", "a")
  val aa_aaa_aa_a = Slice("aa", "aaa", "aa", "a")
  val aa_a_aaa_aa_a = Slice("aa", "a", "aaa", "aa", "a")

  final val id: String => String = x => x

  final def asArrayTree[T: ClassTag](tree: Tree[T]): Tree.ArrayTree[T] =
    tree.deflated.asInstanceOf[Tree.ArrayTree[T]]

  final def treeFrom[T](structure: IntSlice, values: Slice[T]): Tree[T] =
    Transformer.OfTree.fromSlices(structure, values)

  class A
  class B() extends A

  "ArrayTree" should {

    "convert ArrayTree to arrays" in {
      val (s1, v1) = asArrayTree(Tree("a")).toArrays
      s1 shouldBe Array(0)
      v1 shouldBe Array("a")
      val (s2, v2) = asArrayTree(Tree(1, Tree(2, Tree(3), Tree(4)), Tree(5))).toArrays
      s2 shouldBe Array(0, 0, 0, 2, 2)
      v2 shouldBe Array(5, 4, 3, 2, 1)
      val (s3, v3) = asArrayTree(Tree(1d, Tree(4d), Tree(2d, Tree(3d)), Tree(5d))).toArrays
      s3 shouldBe Array(0, 0, 1, 0, 3)
      v3 shouldBe Array(5d, 3d, 2d, 4d, 1d)
    }

    "convert ArrayTree of to structure array" in {
      asArrayTree(Tree("a")).toStructureArray shouldBe Array(0)
      asArrayTree(Tree(1, Tree(2, Tree(3), Tree(4)), Tree(5))).toStructureArray shouldBe Array(0, 0, 0, 2, 2)
      asArrayTree(Tree(1d, Tree(4d), Tree(2d, Tree(3d)), Tree(5d))).toStructureArray shouldBe Array(0, 0, 1, 0, 3)
    }

    "convert ArrayTree of non-primitive type to arrays" in {
      val b1 = new B; val b2 = new B; val b3 = new B; val b4 = new B; val b5 = new B;
      val (s, v) = asArrayTree(Tree(b1, Tree(b2, Tree(b3), Tree(b4)), Tree(b5))).toArrays
      s shouldBe Array(0, 0, 0, 2, 2)
      v shouldBe Array(b5, b4, b3, b2, b1)
    }

    "convert ArrayTree of non-primitive type to buffers" in {
      val b1 = new B; val b2 = new B; val b3 = new B; val b4 = new B; val b5 = new B;
      val (s, v) = asArrayTree(Tree(b1, Tree(b2, Tree(b3), Tree(b4)), Tree(b5))).toBuffers
      s.toArray shouldBe Array(0, 0, 0, 2, 2)
      v.toArray shouldBe Array(b5, b4, b3, b2, b1)

      val (s1, v1) = asArrayTree(Tree(b1, Tree(b2, Tree(b3), Tree(b4)), Tree(b5))).toBuffers[A]
      s1.toArray shouldBe Array(0, 0, 0, 2, 2)
      v1.toArray shouldBe Array(b5, b4, b3, b2, b1)
    }

    "convert ArrayTree of non-primitive type to slices" in {
      val b1 = new B; val b2 = new B; val b3 = new B; val b4 = new B; val b5 = new B;
      val (s, v) = asArrayTree(Tree(b1, Tree(b2, Tree(b3), Tree(b4)), Tree(b5))).toSlices
      s.toArray shouldBe Array(0, 0, 0, 2, 2)
      v.toArray shouldBe Array(b5, b4, b3, b2, b1)
    }

    "convert ArrayTree of non-primitive type to structure array" in {
      val b1 = new B; val b2 = new B; val b3 = new B; val b4 = new B; val b5 = new B;
      val arr = asArrayTree(Tree(b1, Tree(b2, Tree(b3), Tree(b4)), Tree(b5))).toStructureArray
      arr shouldBe Array(0, 0, 0, 2, 2)
    }

    "insert multiple children at once" in {
      insertChildren(
        Tree("a"),
        List(Tree("b", Tree("c", Tree("d")), Tree("f")), Tree("b", Tree("c", Tree("e")), Tree("g"))),
        Nil,
        keepDistinct = true
      ) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e")), Tree("f"), Tree("g")))
      insertChildren(Tree("a", Tree("b")), List(Tree("a"), Tree("b"), Tree("c")), Nil, keepDistinct = true) shouldBe
        Tree("a", Tree("a"), Tree("c"), Tree("b"))
      insertChildren(Tree("a"), List(Tree("b")), List(Tree("c")), keepDistinct = true) shouldBe
        Tree("a", Tree("b"), Tree("c"))
      insertChildren(Tree("a"), List(Tree("b")), List(Tree("c")), keepDistinct = false) shouldBe
        Tree("a", Tree("b"), Tree("c"))
      insertChildren(Tree("a"), List(Tree("b"), Tree("c")), List(Tree("d"), Tree("e")), keepDistinct = true) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("e"))
      insertChildren(Tree("a"), List(Tree("b"), Tree("c")), List(Tree("d"), Tree("e")), keepDistinct = false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("e"))
      insertChildren(Tree("a"), List(Tree("b"), Tree("c")), List(Tree("b"), Tree("d")), keepDistinct = true) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildren(Tree("a"), List(Tree("b"), Tree("c")), List(Tree("b"), Tree("d")), keepDistinct = false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("b"), Tree("d"))
      insertChildren(Tree("a"), Nil, List(Tree("b"), Tree("d")), keepDistinct = true) shouldBe
        Tree("a", Tree("b"), Tree("d"))
      insertChildren(Tree("a"), List(Tree("b"), Tree("c")), Nil, keepDistinct = true) shouldBe
        Tree("a", Tree("b"), Tree("c"))
      insertChildren(Tree("a"), Nil, Nil, keepDistinct = true) shouldBe Tree("a")
      insertChildren[Tree, String, String](Tree.empty, Nil, Nil, keepDistinct = true) shouldBe Tree.empty
      insertChildren(Tree.empty, List(Tree("b")), List(Tree("c")), keepDistinct = true) shouldBe Tree.empty
    }

    "iterate depth-first over tree's values with depth limit" in {
      val all: String => Boolean = _ => true
      val none: String => Boolean = _ => false
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), all, 1, true).toList shouldBe List("a")
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), none, 1, true).toList shouldBe Nil
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 1, true).toList shouldBe List("a")
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 2, true).toList shouldBe List("a", "b")
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 1, true).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 2, true).toList shouldBe List(
        "a",
        "b"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 3, true).toList shouldBe List(
        "a",
        "b",
        "c"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), none, 3, true).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 1, true).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 2, true).toList shouldBe List(
        "a",
        "b",
        "c"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), none, 2, true).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 1, true).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 2, true).toList shouldBe List(
        "a",
        "b",
        "d"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 3, true).toList shouldBe List(
        "a",
        "b",
        "c",
        "d"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 0, true).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 1, true).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 2, true).toList shouldBe List(
        "a",
        "b",
        "c"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 3, true).toList shouldBe List(
        "a",
        "b",
        "c",
        "d"
      )
    }

    "iterate breadth-first over tree's values with depth limit" in {
      val all: String => Boolean = _ => true
      val none: String => Boolean = _ => false
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), all, 1, false).toList shouldBe List("a")
      valuesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), none, 1, false).toList shouldBe Nil
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 1, false).toList shouldBe List("a")
      valuesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), all, 2, false).toList shouldBe List(
        "a",
        "b"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 1, false).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 2, false).toList shouldBe List(
        "a",
        "b"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), all, 3, false).toList shouldBe
        List("a", "b", "c")
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), none, 3, false).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 1, false).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), all, 2, false).toList shouldBe
        List("a", "b", "c")
      valuesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), none, 2, false).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 1, false).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 2, false).toList shouldBe
        List("a", "b", "d")
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), all, 3, false).toList shouldBe
        List("a", "b", "d", "c")
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 0, false).toList shouldBe Nil
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 1, false).toList shouldBe List(
        "a"
      )
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 2, false).toList shouldBe
        List("a", "b", "c")
      valuesIteratorWithLimit(3, treeFrom(IntSlice(0, 1, 0, 2), Slice("d", "c", "b", "a")), all, 3, false).toList shouldBe
        List("a", "b", "c", "d")
    }

    "iterate over tree's branches" in {
      val v = "abcdefghijklmnopqrtuvxyz".apply(_)
      branchesIterator(0, treeFrom(IntSlice(0), RangeMapSlice(v))).map(_.toList).toList shouldBe List(List('a'))
      branchesIterator(1, treeFrom(IntSlice(0, 1), RangeMapSlice(v))).map(_.toList).toList shouldBe List(List('b', 'a'))
      branchesIterator(2, treeFrom(IntSlice(0, 1, 1), RangeMapSlice(v))).map(_.toList).toList shouldBe List(
        List('c', 'b', 'a')
      )
      branchesIterator(2, treeFrom(IntSlice(0, 0, 2), RangeMapSlice(v))).map(_.toList).toList shouldBe List(
        List('c', 'b'),
        List('c', 'a')
      )
      branchesIterator(3, treeFrom(IntSlice(0, 0, 0, 3), RangeMapSlice(v))).map(_.toList).toList shouldBe List(
        List('d', 'c'),
        List('d', 'b'),
        List('d', 'a')
      )
      branchesIterator(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v))).map(_.toList).toList shouldBe List(
        List('d', 'c', 'b'),
        List('d', 'c', 'a')
      )
      branchesIterator(8, treeFrom(IntSlice(0, 1, 0, 1, 0, 2, 0, 2, 2), RangeMapSlice(v)))
        .map(_.toList)
        .toList shouldBe List(
        List('i', 'h', 'g'),
        List('i', 'h', 'f', 'e'),
        List('i', 'h', 'f', 'd', 'c'),
        List('i', 'b', 'a')
      )
    }

    "iterate over tree's branches as values lists without filter" in {
      val v: Int => Int = _ * 10
      val f: Iterable[Int] => Boolean = _ => true
      branchesIteratorWithLimit(0, treeFrom(IntSlice(0), RangeMapSlice(v)), f, 0).map(_.toList).toList shouldBe Nil
      branchesIteratorWithLimit(0, treeFrom(IntSlice(0), RangeMapSlice(v)), f, 10).map(_.toList).toList shouldBe List(
        List(0)
      )
      branchesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), RangeMapSlice(v)), f, 10)
        .map(_.toList)
        .toList shouldBe List(List(10, 0))
      branchesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), RangeMapSlice(v)), f, 10)
        .map(_.toList)
        .toList shouldBe List(List(20, 10, 0))
      branchesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), RangeMapSlice(v)), f, 10)
        .map(_.toList)
        .toList shouldBe List(
        List(20, 10),
        List(20, 0)
      )
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 0, 3), RangeMapSlice(v)), f, 10)
        .map(_.toList)
        .toList shouldBe List(
        List(30, 20),
        List(30, 10),
        List(30, 0)
      )
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v)), f, 10)
        .map(_.toList)
        .toList shouldBe List(
        List(30, 20, 10),
        List(30, 20, 0)
      )
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v)), f, 0)
        .map(_.toList)
        .toList shouldBe Nil
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v)), f, 1)
        .map(_.toList)
        .toList shouldBe List(List(30))
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v)), f, 2)
        .map(_.toList)
        .toList shouldBe List(List(30, 20))
      branchesIteratorWithLimit(3, treeFrom(IntSlice(0, 0, 2, 1), RangeMapSlice(v)), f, 3)
        .map(_.toList)
        .toList shouldBe List(
        List(30, 20, 10),
        List(30, 20, 0)
      )
    }

    "count branches" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _ => true
      countBranches(-1, treeFrom(IntSlice.empty, Slice.empty[String]), f) shouldBe 0
      countBranches(0, treeFrom(IntSlice(0), RangeMapSlice(v)), f) shouldBe 1
      countBranches(0, treeFrom(IntSlice(0, 1), RangeMapSlice(v)), f) shouldBe 1
      countBranches(1, treeFrom(IntSlice(0, 1), RangeMapSlice(v)), f) shouldBe 1
      countBranches(2, treeFrom(IntSlice(0, 1, 1), RangeMapSlice(v)), f) shouldBe 1
      countBranches(2, treeFrom(IntSlice(0, 0, 2), RangeMapSlice(v)), f) shouldBe 2
      countBranches(3, treeFrom(IntSlice(0, 0, 0, 3), RangeMapSlice(v)), f) shouldBe 3
      countBranches(3, treeFrom(IntSlice(0, 0, 1, 2), RangeMapSlice(v)), f) shouldBe 2
      countBranches(2, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2), RangeMapSlice(v)), f) shouldBe 2
      countBranches(7, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2), RangeMapSlice(v)), f) shouldBe 4
      countBranches(9, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), RangeMapSlice(v)), f) shouldBe 5
    }

    "count branches fulfilling the predicate" in {
      val v: Int => String = _.toString
      val f: Iterable[String] => Boolean = _.size > 2
      countBranches(-1, treeFrom(IntSlice.empty, Slice.empty[String]), f) shouldBe 0
      countBranches(0, treeFrom(IntSlice(0), RangeMapSlice(v)), f) shouldBe 0
      countBranches(0, treeFrom(IntSlice(0, 1), RangeMapSlice(v)), f) shouldBe 0
      countBranches(1, treeFrom(IntSlice(0, 1), RangeMapSlice(v)), f) shouldBe 0
      countBranches(2, treeFrom(IntSlice(0, 1, 1), RangeMapSlice(v)), f) shouldBe 1
      countBranches(2, treeFrom(IntSlice(0, 0, 2), RangeMapSlice(v)), f) shouldBe 0
      countBranches(3, treeFrom(IntSlice(0, 0, 0, 3), RangeMapSlice(v)), f) shouldBe 0
      countBranches(3, treeFrom(IntSlice(0, 0, 1, 2), RangeMapSlice(v)), f) shouldBe 1
      countBranches(2, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2), RangeMapSlice(v)), f) shouldBe 0
      countBranches(7, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2), RangeMapSlice(v)), f) shouldBe 4
      countBranches(9, treeFrom(IntSlice(0, 0, 2, 0, 0, 1, 2, 2, 0, 2), RangeMapSlice(v)), f) shouldBe 4
    }

    "access a tree at the index" in {
      instanceAt(0, treeFrom(IntSlice(0), Slice("a"))).height shouldBe 1
      instanceAt(1, treeFrom(IntSlice(0, 1), Slice("a", "b"))).height shouldBe 2
      instanceAt(0, treeFrom(IntSlice(0, 1), Slice("a", "b"))).height shouldBe 1
      instanceAt(2, treeFrom(IntSlice(0, 1, 1), Slice("a", "b", "c"))).height shouldBe 3
      instanceAt(1, treeFrom(IntSlice(0, 1, 1), Slice("a", "b", "c"))).height shouldBe 2
      instanceAt(0, treeFrom(IntSlice(0, 1, 1), Slice("a", "b", "c"))).height shouldBe 1
    }

    "iterate over all trees depth-first" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treesIterator(0, treeFrom(IntSlice(0), Slice("a")), true).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIterator(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), true).toList shouldBe List(
        Tree("a", Tree("b")),
        Tree("b")
      )
      treesIterator(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIterator(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), true).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIterator(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), true).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIterator(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), true).toList shouldBe List(Tree("b"))
      treesIterator(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), true).toList shouldBe List(Tree("c"))
      treesIterator(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), true).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "iterate over all trees breadth-first" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treesIterator(0, treeFrom(IntSlice(0), Slice("a")), false).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIterator(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), false).toList shouldBe List(
        Tree("a", Tree("b")),
        Tree("b")
      )
      treesIterator(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), false).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIterator(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), false).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIterator(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIterator(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), false).toList shouldBe List(Tree("b"))
      treesIterator(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), false).toList shouldBe List(Tree("c"))
      treesIterator(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), false).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "iterate over filtered trees depth-first" in {
      val f: Tree[String] => Boolean = _.size % 2 != 0
      val l = treesIteratorWithFilter(0, treeFrom(IntSlice(0), Slice("a")), f, true).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), f, true).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("c")
      )
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, true).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, true).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, true).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithFilter(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, true).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), f, true).toList shouldBe List(
        Tree("d")
      )
    }

    "iterate over filtered trees breadth-first" in {
      val f: Tree[String] => Boolean = _.size % 2 != 0
      val l = treesIteratorWithFilter(0, treeFrom(IntSlice(0), Slice("a")), f, false).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), f, false).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, false).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("c")
      )
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, false).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithFilter(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, false).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithFilter(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, false).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithFilter(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), f, false).toList shouldBe List(
        Tree("d")
      )
    }

    "iterate over filtered trees depth-first with depth limit" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), f, 2, true).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), f, 2, true).toList shouldBe List(
        Tree("a", Tree("b")),
        Tree("b")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c"))
      )
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithLimit(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), f, 2, true).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "iterate over filtered trees breadth-first with depth limit" in {
      val f: Tree[String] => Boolean = _ => true
      val l = treesIteratorWithLimit(0, treeFrom(IntSlice(0), Slice("a")), f, 2, false).toList
      l shouldBe List(Tree("a"))
      l should not be List(Tree("b"))
      l should not be List(Tree("a", Tree("b")))
      l should not be Nil
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 1), Slice("b", "a")), f, 2, false).toList shouldBe List(
        Tree("a", Tree("b")),
        Tree("b")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("a", Tree("b", Tree("c"))),
        Tree("b", Tree("c"))
      )
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 1, 1), Slice("c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("c")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("a", Tree("b"), Tree("c")),
        Tree("b"),
        Tree("c")
      )
      treesIteratorWithLimit(1, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("b")
      )
      treesIteratorWithLimit(0, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("c")
      )
      treesIteratorWithLimit(2, treeFrom(IntSlice(0, 0, 1, 2, 1), Slice("e", "d", "c", "b", "a")), f, 2, false).toList shouldBe List(
        Tree("c", Tree("d")),
        Tree("d")
      )
    }

    "select a value by the path" in {
      selectValue(List("a"), -1, treeFrom(IntSlice.empty, Slice.empty[String]), id, rightmost = true) shouldBe None
      selectValue(List("a"), 0, treeFrom(IntSlice(0), Slice("a")), id, rightmost = true) shouldBe Some("a")
      selectValue(List("a", "b"), 0, treeFrom(IntSlice(0), Slice("a")), id, rightmost = true) shouldBe None
      selectValue(List("a", "a"), 0, treeFrom(IntSlice(0), Slice("a")), id, rightmost = true) shouldBe None
      selectValue(List("a", "b"), 0, treeFrom(IntSlice(0, 1), Slice("b", "a")), id, rightmost = true) shouldBe None
      selectValue(List("a", "b"), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), id, rightmost = true) shouldBe Some("b")
      selectValue(List("a", "b"), 1, treeFrom(IntSlice(0, 1), Slice("c", "a")), id, rightmost = true) shouldBe None
      selectValue(List("a", "b"), 2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), id, rightmost = true) shouldBe Some(
        "b"
      )
      selectValue(List("a", "c"), 2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), id, rightmost = true) shouldBe Some(
        "c"
      )
      selectValue(List("a", "d"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), id, rightmost = true) shouldBe Some(
        "d"
      )
      selectValue(
        List("a", "b", "c"),
        3,
        treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")),
        id,
        rightmost = true
      ) shouldBe Some(
        "c"
      )
      selectValue(
        List("a", "b", "c"),
        3,
        treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")),
        id,
        rightmost = true
      ) shouldBe Some(
        "c"
      )
      selectValue(List("a", "b"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), id, rightmost = true) shouldBe Some(
        "b"
      )
      selectValue(List("a", "c"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), id, rightmost = true) shouldBe None
    }

    "select a tree by the path" in {
      selectTree(List("a"), -1, treeFrom(IntSlice.empty, Slice.empty[String]), rightmost = false) shouldBe None
      selectTree(List("a"), 0, treeFrom(IntSlice(0), Slice("a")), rightmost = false) shouldBe Some(Tree("a"))
      selectTree(List("a", "b"), 0, treeFrom(IntSlice(0), Slice("a")), rightmost = false) shouldBe None
      selectTree(List("a", "a"), 0, treeFrom(IntSlice(0), Slice("a")), rightmost = false) shouldBe None
      selectTree(List("a", "b"), 0, treeFrom(IntSlice(0, 1), Slice("b", "a")), rightmost = false) shouldBe None
      selectTree(List("a", "b"), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), rightmost = false) shouldBe Some(
        Tree("b")
      )
      selectTree(List("a", "b"), 1, treeFrom(IntSlice(0, 1), Slice("c", "a")), rightmost = false) shouldBe None
      selectTree(List("a", "b"), 2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("b")
      )
      selectTree(List("a", "c"), 2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("c")
      )
      selectTree(List("a", "d"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("d")
      )
      selectTree(List("a", "b", "c"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("c")
      )
      selectTree(List("a", "b", "c"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("c")
      )
      selectTree(List("a", "b"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), rightmost = false) shouldBe Some(
        Tree("b", Tree("c"))
      )
      selectTree(List("a", "c"), 3, treeFrom(IntSlice(0, 0, 1, 2), Slice("d", "c", "b", "a")), rightmost = false) shouldBe None
    }

    "select a tree by the path using extractor function" in {
      val length: String => Int = (s: String) => s.length
      selectTree(List(1), -1, treeFrom(IntSlice.empty, Slice.empty[String]), length, rightmost = false) shouldBe None
      selectTree(List(1), 0, treeFrom(IntSlice(0), Slice("a")), length, rightmost = false) shouldBe Some(Tree("a"))
      selectTree(List(0), 0, treeFrom(IntSlice(0), Slice("a")), length, rightmost = false) shouldBe None
      selectTree(List(1), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), length, rightmost = false) shouldBe Some(
        Tree("a", Tree("b"))
      )
      selectTree(List(1, 1), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), length, rightmost = false) shouldBe Some(
        Tree("b")
      )
      selectTree(List(1, 0), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), length, rightmost = false) shouldBe None
      selectTree(List(0, 1), 1, treeFrom(IntSlice(0, 1), Slice("b", "a")), length, rightmost = false) shouldBe None
      selectTree(List(1, 1), 2, treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), length, rightmost = false) shouldBe Some(
        Tree("b")
      )
      selectTree(
        List(1, 2, 3),
        4,
        treeFrom(IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a")),
        length,
        rightmost = false
      ) shouldBe Some(
        Tree("aaa")
      )
      selectTree(
        List(1, 4),
        4,
        treeFrom(IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a")),
        length,
        rightmost = false
      ) shouldBe Some(
        Tree("aaaa", Tree("aaaaa"))
      )
      selectTree(
        List(1, 2),
        4,
        treeFrom(IntSlice(0, 1, 0, 1, 2), Slice("aaaaa", "aaaa", "aaa", "aa", "a")),
        length,
        rightmost = false
      ) shouldBe Some(
        Tree("aa", Tree("aaa"))
      )
    }

    "flatMap a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMapLax(treeFrom(IntSlice.empty, Slice.empty[String]), f0) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f0) shouldBe Tree(1)
      flatMapLax(treeFrom(IntSlice(0, 1), aa_a), f0) shouldBe Tree(1, Tree(2))
      flatMapLax(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMapLax(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMapLax(treeFrom(IntSlice(0, 1, 1, 1), a_aaa_aa_a), f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMapLax(treeFrom(IntSlice(0, 0, 0, 3), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMapLax(treeFrom(IntSlice(0, 0, 2, 1), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMapLax(treeFrom(IntSlice(0, 0, 1, 2), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMapLax(treeFrom(IntSlice(0, 1, 0, 2), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMapLax(treeFrom(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a), f0) shouldBe Tree(
        1,
        Tree(2, Tree(3), Tree(1, Tree(2)))
      )
      flatMapLax(treeFrom(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a), f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMapLax(treeFrom(IntSlice.empty, Slice.empty[String]), f1) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f1) shouldBe Tree(1, Tree(2))
      flatMapLax(treeFrom(IntSlice(0, 1), aa_a), f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(2))
      flatMapLax(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(2)
      )
      flatMapLax(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(2)
      )

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMapLax(treeFrom(IntSlice.empty, Slice.empty[String]), f2) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f2) shouldBe Tree(1, Tree(2), Tree(2))
      flatMapLax(treeFrom(IntSlice(0, 1), aa_a), f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)), Tree(2), Tree(2))
      flatMapLax(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4), Tree(3)),
        Tree(2),
        Tree(2)
      )

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMapLax(treeFrom(IntSlice.empty, Slice.empty[String]), f3) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f3) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0, 1), aa_a), f3) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMapLax(treeFrom(IntSlice.empty, Slice.empty[String]), f4) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f4) shouldBe Tree.empty
      flatMapLax(treeFrom(IntSlice(0, 1), aa_a), f4) shouldBe Tree(2, Tree(4))
      flatMapLax(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))

      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapLax(treeFrom(IntSlice(0), Slice("a")), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapLax(treeFrom(IntSlice(0, 1), Slice("b", "a")), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapLax(treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f5) shouldBe
        Tree(
          0,
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))),
          Tree(1, Tree(2, Tree(4, Tree(5))))
        )
    }

    "flatMap distinct a tree" in {
      val f0: String => Tree[Int] = s => Tree(s.length)
      flatMapDistinct(treeFrom(IntSlice.empty, Slice.empty[String]), f0) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f0) shouldBe Tree(1)
      flatMapDistinct(treeFrom(IntSlice(0, 1), aa_a), f0) shouldBe Tree(1, Tree(2))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3)))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1, 1), a_aaa_aa_a), f0) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1)))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 0, 3), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3), Tree(1))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2, 1), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3), Tree(1)))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 1, 2), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2, Tree(3)), Tree(1))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2), a_aaa_aa_a), f0) shouldBe Tree(1, Tree(2), Tree(3, Tree(1)))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a), f0) shouldBe Tree(
        1,
        Tree(2, Tree(3), Tree(1, Tree(2)))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a), f0) shouldBe
        Tree(1, Tree(2, Tree(3)), Tree(1, Tree(2)))

      val f1: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(treeFrom(IntSlice.empty, Slice.empty[String]), f1) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f1) shouldBe Tree(1, Tree(2))
      flatMapDistinct(treeFrom(IntSlice(0, 1), aa_a), f1) shouldBe Tree(1, Tree(2, Tree(4)))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f1) shouldBe Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f1) shouldBe Tree(1, Tree(2, Tree(4)), Tree(3, Tree(6)))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1, 1), a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(1, Tree(2)), Tree(6)), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 0, 3), a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(6)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2, 1), a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 1, 2), a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2), a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(4)),
        Tree(3, Tree(1, Tree(2)), Tree(6))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a), f1) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6)), Tree(1, Tree(2, Tree(4))), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a), f1) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6)), Tree(4)), Tree(1, Tree(2, Tree(4))))

      val f2: String => Tree[Int] = s => Tree(s.length, Tree(s.length * 2), Tree(s.length + 1))
      flatMapDistinct(treeFrom(IntSlice.empty, Slice.empty[String]), f2) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f2) shouldBe Tree(1, Tree(2))
      flatMapDistinct(treeFrom(IntSlice(0, 1), aa_a), f2) shouldBe Tree(1, Tree(2, Tree(4), Tree(3)))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(6), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 0, 3), aa_aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(3, Tree(6), Tree(4)),
        Tree(2, Tree(4), Tree(3))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2, 1), a_aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2)), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 0, 1, 2), a_aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)),
        Tree(1, Tree(2))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2), a_aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(4), Tree(3)),
        Tree(3, Tree(1, Tree(2)), Tree(6), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a), f2) shouldBe Tree(
        1,
        Tree(2, Tree(3, Tree(6), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))), Tree(4))
      )
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a), f2) shouldBe
        Tree(1, Tree(2, Tree(3, Tree(6), Tree(4)), Tree(4)), Tree(1, Tree(2, Tree(4), Tree(3))))

      val f3: String => Tree[Int] = _ => Tree.empty
      flatMapDistinct(treeFrom(IntSlice.empty, Slice.empty[String]), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1), aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1, 1), a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 0, 0, 3), a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2, 1), a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 0, 1, 2), a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2), a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 2, 1), aa_a_aaa_aa_a), f3) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1, 0, 1, 2), aa_a_aaa_aa_a), f3) shouldBe Tree.empty

      val f4: String => Tree[Int] = s => if (s == "a") Tree.empty else Tree(s.length, Tree(s.length * 2))
      flatMapDistinct(treeFrom(IntSlice.empty, Slice.empty[String]), f4) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f4) shouldBe Tree.empty
      flatMapDistinct(treeFrom(IntSlice(0, 1), aa_a), f4) shouldBe Tree(2, Tree(4))
      flatMapDistinct(treeFrom(IntSlice(0, 1, 1), aaa_aa_a), f4) shouldBe Tree(2, Tree(3, Tree(6)), Tree(4))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), aaa_aa_a), f4) shouldBe Tree.empty

      // test direct subtree insertion
      val f5: String => Tree[Int] = _ => Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(treeFrom(IntSlice(0), Slice("a")), f5) shouldBe Tree(0, Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(treeFrom(IntSlice(0, 1), Slice("b", "a")), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
      flatMapDistinct(treeFrom(IntSlice(0, 0, 2), Slice("c", "b", "a")), f5) shouldBe
        Tree(0, Tree(0, Tree(1, Tree(2, Tree(4, Tree(5))))), Tree(1, Tree(2, Tree(4, Tree(5)))))
    }

    "insert distinct value prepend to existing children" in {
      insertLeaf(0, "a", Tree("a"), false, true) shouldBe Tree("a", Tree("a"))
      insertLeaf(1, "a", Tree("a", Tree("b")), false, true) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertLeaf(0, "a", Tree("a", Tree("b")), false, true) shouldBe Tree("a", Tree("b", Tree("a")))
      insertLeaf(2, "a", Tree("a", Tree("b"), Tree("c")), false, true) shouldBe Tree(
        "a",
        Tree("a"),
        Tree("b"),
        Tree("c")
      )
      insertLeaf(1, "a", Tree("a", Tree("b"), Tree("c")), false, true) shouldBe Tree(
        "a",
        Tree("b", Tree("a")),
        Tree("c")
      )
      insertLeaf(0, "a", Tree("a", Tree("b"), Tree("c")), false, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("a"))
      )
      insertLeaf(0, "a", Tree("a", Tree("b", Tree("c"))), false, true) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a")))
      )
      insertLeaf(1, "a", Tree("a", Tree("b", Tree("c"))), false, true) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c"))
      )
      insertLeaf(2, "a", Tree("a", Tree("b", Tree("c"))), false, true) shouldBe Tree(
        "a",
        Tree("a"),
        Tree("b", Tree("c"))
      )
      insertLeaf(2, "b", Tree("a", Tree("b", Tree("c"))), false, true) shouldBe
        Tree("a", Tree("b", Tree("c")))
    }

    "insert lax value - prepend to existing children" in {
      insertLeaf(0, "a", Tree("a"), false, false) shouldBe Tree("a", Tree("a"))
      insertLeaf(1, "a", Tree("a", Tree("b")), false, false) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertLeaf(0, "a", Tree("a", Tree("b")), false, false) shouldBe Tree("a", Tree("b", Tree("a")))
      insertLeaf(2, "a", Tree("a", Tree("b"), Tree("c")), false, false) shouldBe Tree(
        "a",
        Tree("a"),
        Tree("b"),
        Tree("c")
      )
      insertLeaf(1, "a", Tree("a", Tree("b"), Tree("c")), false, false) shouldBe Tree(
        "a",
        Tree("b", Tree("a")),
        Tree("c")
      )
      insertLeaf(0, "a", Tree("a", Tree("b"), Tree("c")), false, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("a"))
      )
      insertLeaf(0, "a", Tree("a", Tree("b", Tree("c"))), false, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a")))
      )
      insertLeaf(1, "a", Tree("a", Tree("b", Tree("c"))), false, false) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c"))
      )
      insertLeaf(2, "a", Tree("a", Tree("b", Tree("c"))), false, false) shouldBe Tree(
        "a",
        Tree("a"),
        Tree("b", Tree("c"))
      )
      insertLeaf(2, "b", Tree("a", Tree("b", Tree("c"))), false, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c"))
      )
    }

    "insert distinct value append to existing children" in {
      insertLeaf(0, "a", Tree("a"), true, true) shouldBe Tree("a", Tree("a"))
      insertLeaf(1, "a", Tree("a", Tree("b")), true, true) shouldBe
        Tree("a", Tree("b"), Tree("a"))
      insertLeaf(0, "a", Tree("a", Tree("b")), true, true) shouldBe
        Tree("a", Tree("b", Tree("a")))
      insertLeaf(2, "a", Tree("a", Tree("b"), Tree("c")), true, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("a")
      )
      insertLeaf(1, "a", Tree("a", Tree("b"), Tree("c")), true, true) shouldBe Tree(
        "a",
        Tree("b", Tree("a")),
        Tree("c")
      )
      insertLeaf(0, "a", Tree("a", Tree("b"), Tree("c")), true, true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("a"))
      )
      insertLeaf(0, "a", Tree("a", Tree("b", Tree("c"))), true, true) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a")))
      )
      insertLeaf(1, "a", Tree("a", Tree("b", Tree("c"))), true, true) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("a"))
      )
      insertLeaf(2, "a", Tree("a", Tree("b", Tree("c"))), true, true) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("a")
      )
      insertLeaf(2, "b", Tree("a", Tree("b", Tree("c"))), true, true) shouldBe
        Tree("a", Tree("b", Tree("c")))
    }

    "insert lax value - append to existing children" in {
      insertLeaf(0, "a", Tree("a"), true, false) shouldBe Tree("a", Tree("a"))
      insertLeaf(1, "a", Tree("a", Tree("b")), true, false) shouldBe Tree("a", Tree("b"), Tree("a"))
      insertLeaf(0, "a", Tree("a", Tree("b")), true, false) shouldBe Tree("a", Tree("b", Tree("a")))
      insertLeaf(2, "a", Tree("a", Tree("b"), Tree("c")), true, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("a")
      )
      insertLeaf(1, "a", Tree("a", Tree("b"), Tree("c")), true, false) shouldBe Tree(
        "a",
        Tree("b", Tree("a")),
        Tree("c")
      )
      insertLeaf(0, "a", Tree("a", Tree("b"), Tree("c")), true, false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("a"))
      )
      insertLeaf(0, "a", Tree("a", Tree("b", Tree("c"))), true, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a")))
      )
      insertLeaf(1, "a", Tree("a", Tree("b", Tree("c"))), true, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("a"))
      )
      insertLeaf(2, "a", Tree("a", Tree("b", Tree("c"))), true, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("a")
      )
      insertLeaf(2, "b", Tree("a", Tree("b", Tree("c"))), true, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("b")
      )
    }

    "insert a subtree" in {
      insertTreeAtIndex[Tree, String, String](0, 0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertTreeAtIndex(0, 0, Tree("a"), Tree.empty) shouldBe Tree("a")
      insertTreeAtIndex(0, 0, Tree.empty, Tree("a")) shouldBe Tree("a")
      insertTreeAtIndex(0, 0, Tree("a"), Tree("a")) shouldBe Tree("a", Tree("a"))
      insertTreeAtIndex(1, 1, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertTreeAtIndex(0, 0, Tree("a"), Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b", Tree("a")))
      insertTreeAtIndex(1, 1, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree(
        "a",
        Tree("b", Tree("a")),
        Tree("b")
      )
      insertTreeAtIndex(0, 0, Tree("b", Tree("a")), Tree("a", Tree("b"))) shouldBe Tree(
        "a",
        Tree("b", Tree("b", Tree("a")))
      )
      insertTreeAtIndex(2, 2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b")))) shouldBe Tree(
        "b",
        Tree("a", Tree("b")),
        Tree("a", Tree("b"))
      )
    }

    "insert a child distinct - prepend to existing" in {
      insertChildDistinct[Tree, String, String](0, Tree.empty, Tree.empty, append = false) shouldBe Tree.empty
      insertChildDistinct(0, Tree("a"), Tree.empty, append = false) shouldBe Tree("a")
      insertChildDistinct(0, Tree.empty, Tree("a"), append = false) shouldBe Tree("a")
      insertChildDistinct(0, Tree("a"), Tree("a"), append = false) shouldBe Tree("a", Tree("a"))
      insertChildDistinct(1, Tree("a"), Tree("a", Tree("b")), append = false) shouldBe Tree("a", Tree("a"), Tree("b"))
      insertChildDistinct(0, Tree("a"), Tree("a", Tree("b")), append = false) shouldBe Tree("a", Tree("b", Tree("a")))
      insertChildDistinct(1, Tree("b", Tree("a")), Tree("a", Tree("b")), append = false) shouldBe Tree(
        "a",
        Tree("b", Tree("a"))
      )
      insertChildDistinct(0, Tree("b", Tree("a")), Tree("a", Tree("b")), append = false) shouldBe Tree(
        "a",
        Tree("b", Tree("b", Tree("a")))
      )
      insertChildDistinct(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b"))), append = false) shouldBe Tree(
        "b",
        Tree("a", Tree("b"))
      )
      insertChildDistinct(2, Tree("b", Tree("d"), Tree("e")), Tree("a", Tree("b"), Tree("c")), append = false) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
    }

    "insert a child distinct - append to existing" in {
      //insertSubtreeDistinct(0, Tree.empty, Tree.empty) shouldBe Tree.empty
      insertChildDistinct(0, Tree("a"), Tree.empty, append = true) shouldBe Tree("a")
      insertChildDistinct(0, Tree.empty, Tree("a"), append = true) shouldBe Tree("a")
      insertChildDistinct(0, Tree("a"), Tree("a"), append = true) shouldBe Tree("a", Tree("a"))
      insertChildDistinct(1, Tree("a"), Tree("a", Tree("b")), append = true) shouldBe Tree("a", Tree("b"), Tree("a"))
      insertChildDistinct(0, Tree("a"), Tree("a", Tree("b")), append = true) shouldBe Tree("a", Tree("b", Tree("a")))
      insertChildDistinct(1, Tree("b", Tree("a")), Tree("a", Tree("b")), append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("a"))
      )
      insertChildDistinct(0, Tree("b", Tree("a")), Tree("a", Tree("b")), append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("b", Tree("a")))
      )
      insertChildDistinct(2, Tree("a", Tree("b")), Tree("b", Tree("a", Tree("b"))), append = true) shouldBe Tree(
        "b",
        Tree("a", Tree("b"))
      )
      insertChildDistinct(2, Tree("b", Tree("d"), Tree("e")), Tree("a", Tree("b"), Tree("c")), append = true) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
    }

    "insert a branch into the tree" in {
      //insertBranch(-1, List(), Tree.empty) shouldBe Tree.empty
      insertBranch(-1, List("a"), Tree.empty, append = false) shouldBe Tree("a")
      insertBranch(-1, List("a", "b", "c", "d", "e"), Tree.empty, append = false) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d", Tree("e"))))
      )
      insertBranch(0, List("a"), Tree("a"), append = false) shouldBe Tree("a")
      insertBranch(1, List("a"), Tree("a", Tree("b")), append = false) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "b"), Tree("a", Tree("b")), append = false) shouldBe Tree("a", Tree("b"))
      insertBranch(1, List("a", "c"), Tree("a", Tree("b")), append = false) shouldBe Tree("a", Tree("c"), Tree("b"))
      insertBranch(3, List("a", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d")), append = false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "c", "e"), Tree("a", Tree("b"), Tree("c"), Tree("d")), append = false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("e")),
        Tree("d")
      )
      insertBranch(3, List("a", "b", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d")), append = false) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      insertBranch(3, List("a", "d", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d")), append = false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d", Tree("c"))
      )
      insertBranch(3, List("a", "e", "c"), Tree("a", Tree("b"), Tree("c"), Tree("d")), append = false) shouldBe Tree(
        "a",
        Tree("e", Tree("c")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
    }

    "update value at the index" in {
      updateValue(0, "c", Tree("a"), false) shouldBe Tree("c")
      updateValue(0, "c", Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("c"))
      updateValue(1, "c", Tree("a", Tree("b")), false) shouldBe Tree("c", Tree("b"))
      updateValue(0, "c", Tree("a", Tree("b"), Tree("d")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c")
      )
      updateValue(1, "c", Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("c"),
        Tree("c")
      )
      updateValue(2, "c", Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "c",
        Tree("b"),
        Tree("c")
      )

    }

    "update distinct value at the index" in {
      updateValue(0, "c", Tree("a"), true) shouldBe Tree("c")
      updateValue(0, "c", Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("c"))
      updateValue(1, "c", Tree("a", Tree("b")), true) shouldBe Tree("c", Tree("b"))
      updateValue(0, "c", Tree("a", Tree("b"), Tree("d")), true) shouldBe Tree("a", Tree("b"), Tree("c"))
      updateValue(1, "c", Tree("a", Tree("b"), Tree("c")), true) shouldBe
        Tree("a", Tree("c"))
      updateValue(2, "c", Tree("a", Tree("b"), Tree("c")), true) shouldBe
        Tree("c", Tree("b"), Tree("c"))
    }

    "modify value at the index" in {
      val f: String => String = (_: String) => "c"
      modifyValue(0, f, Tree("a"), false) shouldBe Tree("c")
      modifyValue(0, f, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("c"))
      modifyValue(1, f, Tree("a", Tree("b")), false) shouldBe Tree("c", Tree("b"))
      modifyValue(0, f, Tree("a", Tree("b"), Tree("d")), false) shouldBe Tree("a", Tree("b"), Tree("c"))
      modifyValue(1, f, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("c"), Tree("c"))
      modifyValue(2, f, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("c", Tree("b"), Tree("c"))
    }

    "modify distinct value at the index" in {
      val f: String => String = (_: String) => "c"
      modifyValue(0, f, Tree("a"), true) shouldBe Tree("c")
      modifyValue(0, f, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("c"))
      modifyValue(1, f, Tree("a", Tree("b")), true) shouldBe Tree("c", Tree("b"))
      modifyValue(0, f, Tree("a", Tree("b"), Tree("d")), true) shouldBe Tree("a", Tree("b"), Tree("c"))
      modifyValue(1, f, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("c"))
      modifyValue(2, f, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("c", Tree("b"), Tree("c"))
    }

    "remove value at index" in {
      removeValue(0, None, Tree("a"), false) shouldBe Tree.empty
      removeValue(-1, None, Tree("a"), false) shouldBe Tree("a")
      removeValue(1, None, Tree("a"), false) shouldBe Tree("a")
      removeValue(1, None, Tree("a", Tree("b")), false) shouldBe Tree("b")
      removeValue(0, None, Tree("a", Tree("b")), false) shouldBe Tree("a")
      removeValue(0, None, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b"))
      removeValue(1, None, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("c"))
      a[RuntimeException] shouldBe thrownBy {
        removeValue(2, None, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("b")
      }
      removeValue(4, None, Tree("a", Tree("b", Tree("c")), Tree("c", Tree("e", Tree("f")))), false) shouldBe Tree(
        "a",
        Tree("c"),
        Tree("c", Tree("e", Tree("f")))
      )
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e", Tree("f")))),
        false
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g"))), Tree("c", Tree("e", Tree("f"))))
    }

    "remove distinct value at index" in {
      removeValue(0, None, Tree("a"), true) shouldBe Tree.empty
      removeValue(-1, None, Tree("a"), true) shouldBe Tree("a")
      removeValue(1, None, Tree("a"), true) shouldBe Tree("a")
      removeValue(1, None, Tree("a", Tree("b")), true) shouldBe Tree("b")
      removeValue(0, None, Tree("a", Tree("b")), true) shouldBe Tree("a")
      removeValue(0, None, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("b"))
      removeValue(1, None, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("c"))
      a[RuntimeException] shouldBe thrownBy {
        removeValue(2, None, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("b")
      }
      removeValue(4, None, Tree("a", Tree("b", Tree("c")), Tree("c", Tree("e", Tree("f")))), true) shouldBe Tree(
        "a",
        Tree("c", Tree("e", Tree("f")))
      )
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e", Tree("f")))),
        true
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g"), Tree("f"))))
      removeValue(
        6,
        None,
        Tree("a", Tree("b", Tree("c", Tree("e", Tree("g")))), Tree("c", Tree("e"), Tree("f"))),
        true
      ) shouldBe Tree("a", Tree("c", Tree("e", Tree("g")), Tree("f")))
    }

    "update the tree at the index" in {
      updateTree(0, Tree("b"), Tree("a"), false) shouldBe Tree("b")
      updateTree(0, Tree.empty, Tree("c"), false) shouldBe Tree.empty
      updateTree(0, Tree.empty, Tree("a", Tree("b")), false) shouldBe Tree("a")
      updateTree(1, Tree.empty, Tree("a", Tree("b")), false) shouldBe Tree.empty
      updateTree(0, Tree("c"), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("c"))
      updateTree(1, Tree("c"), Tree("a", Tree("b")), false) shouldBe Tree("c")
      updateTree(0, Tree("b", Tree("a")), Tree("c"), false) shouldBe Tree("b", Tree("a"))
      updateTree(0, Tree("a", Tree("b")), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("a", Tree("b")))
      updateTree(1, Tree("a", Tree("b")), Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("b"))
      updateTree(1, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("a", Tree("b")),
        Tree("c")
      )
      updateTree(0, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("a", Tree("b"))
      )
      updateTree(2, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b"))
      updateTree(0, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("d"))
      )
      updateTree(0, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("b", Tree("d"))
      )
      updateTree(
        1,
        Tree("b", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("b", Tree("d"), Tree("e")), Tree("b"))
      updateTree(
        4,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("c"), Tree("b"))
      updateTree(
        5,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        false
      ) shouldBe Tree("c", Tree("d"), Tree("e"))
    }

    "update distinct the tree at the index" in {
      updateTree(0, Tree("b"), Tree("a"), true) shouldBe Tree("b")
      updateTree(0, Tree.empty, Tree("c"), true) shouldBe Tree.empty
      updateTree(0, Tree.empty, Tree("a", Tree("b")), true) shouldBe Tree("a")
      updateTree(1, Tree.empty, Tree("a", Tree("b")), true) shouldBe Tree.empty
      updateTree(0, Tree("c"), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("c"))
      updateTree(1, Tree("c"), Tree("a", Tree("b")), true) shouldBe Tree("c")
      updateTree(0, Tree("b", Tree("a")), Tree("c"), true) shouldBe Tree("b", Tree("a"))
      updateTree(0, Tree("a", Tree("b")), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("a", Tree("b")))
      updateTree(1, Tree("a", Tree("b")), Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("b"))
      updateTree(1, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("a", Tree("b")),
        Tree("c")
      )
      updateTree(0, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("a", Tree("b"))
      )
      updateTree(2, Tree("a", Tree("b")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("b"))
      updateTree(0, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d"))
      )
      updateTree(0, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("b", Tree("d"))
      )
      updateTree(1, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
      updateTree(1, Tree("b", Tree("d")), Tree("a", Tree("b", Tree("d")), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
      updateTree(
        1,
        Tree("b", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("a", Tree("b", Tree("d"), Tree("f"), Tree("e")), Tree("b"))
      updateTree(
        4,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("b"))
      updateTree(
        5,
        Tree("c", Tree("d"), Tree("e")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("c", Tree("d"), Tree("e"))
      updateTree(
        5,
        Tree("c", Tree("d"), Tree("d")),
        Tree("a", Tree("b", Tree("d"), Tree("f")), Tree("c"), Tree("b")),
        true
      ) shouldBe Tree("c", Tree("d"), Tree("d"))
      updateTree(0, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("d"))
      )
      updateTree(1, Tree("b", Tree("d")), Tree("a", Tree("b"), Tree("c"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
      updateTree(2, Tree("b", Tree("d")), Tree("a", Tree("c"), Tree("b"), Tree("b")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("b")
      )
    }

    "modify the tree at the index" in {
      val t2 = Tree("v", Tree("x"), Tree("y", Tree("z")))
      val f1: Tree[String] => Tree[String] = _.insertLeafLax("z")
      val f2: Tree[String] => Tree[String] = _.insertChildLax(t2)
      val f3: Tree[String] => Tree[String] = t => t.children.headOption.getOrElse(Tree("z")).insertLeafLax(t.head)
      val f4: Tree[String] => Tree[String] = t => Tree.empty

      modifyTree(0, f1, Tree("a"), false) shouldBe Tree("a", Tree("z"))
      modifyTree(0, f1, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("b", Tree("z")))
      modifyTree(1, f1, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("z"), Tree("b"))
      modifyTree(2, f1, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("z"),
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, f1, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b", Tree("z")),
        Tree("c")
      )
      modifyTree(0, f1, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("z"))
      )

      modifyTree(0, f2, Tree("a"), false) shouldBe Tree("a", t2)
      modifyTree(0, f2, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("b", t2))
      modifyTree(1, f2, Tree("a", Tree("b")), false) shouldBe Tree("a", t2, Tree("b"))
      modifyTree(2, f2, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        t2,
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, f2, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b", t2),
        Tree("c")
      )
      modifyTree(0, f2, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", t2)
      )

      modifyTree(0, f3, Tree("a"), false) shouldBe Tree("z", Tree("a"))
      modifyTree(0, f3, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("z", Tree("b")))
      modifyTree(1, f3, Tree("a", Tree("b")), false) shouldBe Tree("b", Tree("a"))
      modifyTree(2, f3, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("b", Tree("a"))
      modifyTree(1, f3, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("z", Tree("b")),
        Tree("c")
      )
      modifyTree(0, f3, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("z", Tree("c"))
      )

      modifyTree(0, f4, Tree("a"), false) shouldBe Tree.empty
      modifyTree(0, f4, Tree("a", Tree("b")), false) shouldBe Tree("a")
      modifyTree(1, f4, Tree("a", Tree("b")), false) shouldBe Tree.empty
      modifyTree(2, f4, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree.empty
      modifyTree(1, f4, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("c"))
      modifyTree(0, f4, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree("a", Tree("b"))

      def previous(s: String): String = s match {
        case "b" => "a"
        case "c" => "b"
        case "d" => "c"
        case _   => "z"
      }
      val f5: Tree[String] => Tree[String] = t => Tree(previous(t.head), Tree("x"), Tree("y"))

      modifyTree(0, f5, Tree("a"), false) shouldBe Tree("z", Tree("x"), Tree("y"))
      modifyTree(0, f5, Tree("a", Tree("b")), false) shouldBe Tree("a", Tree("a", Tree("x"), Tree("y")))
      modifyTree(0, f5, Tree("a", Tree("b"), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("x"), Tree("y"))
      )
      modifyTree(0, f5, Tree("a", Tree("b", Tree("x"), Tree("z")), Tree("c")), false) shouldBe Tree(
        "a",
        Tree("b", Tree("x"), Tree("z")),
        Tree("b", Tree("x"), Tree("y"))
      )
    }

    "modify distinct the tree at the index" in {
      val t2 = Tree("v", Tree("x"), Tree("y", Tree("z")))
      val f1: Tree[String] => Tree[String] = _.insertLeafLax("z")
      val f2: Tree[String] => Tree[String] = _.insertChildLax(t2)
      val f3: Tree[String] => Tree[String] = t => t.children.headOption.getOrElse(Tree("z")).insertLeafLax(t.head)
      val f4: Tree[String] => Tree[String] = t => Tree.empty

      modifyTree(0, f1, Tree("a"), true) shouldBe Tree("a", Tree("z"))
      modifyTree(0, f1, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("b", Tree("z")))
      modifyTree(1, f1, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("z"), Tree("b"))
      modifyTree(2, f1, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("z"),
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, f1, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("z")),
        Tree("c")
      )
      modifyTree(0, f1, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", Tree("z"))
      )

      modifyTree(0, f2, Tree("a"), true) shouldBe Tree("a", t2)
      modifyTree(0, f2, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("b", t2))
      modifyTree(1, f2, Tree("a", Tree("b")), true) shouldBe Tree("a", t2, Tree("b"))
      modifyTree(2, f2, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        t2,
        Tree("b"),
        Tree("c")
      )
      modifyTree(1, f2, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", t2),
        Tree("c")
      )
      modifyTree(0, f2, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c", t2)
      )

      modifyTree(0, f3, Tree("a"), true) shouldBe Tree("z", Tree("a"))
      modifyTree(0, f3, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("z", Tree("b")))
      modifyTree(1, f3, Tree("a", Tree("b")), true) shouldBe Tree("b", Tree("a"))
      modifyTree(2, f3, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("b", Tree("a"))
      modifyTree(1, f3, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("z", Tree("b")),
        Tree("c")
      )
      modifyTree(0, f3, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("z", Tree("c"))
      )

      modifyTree(0, f4, Tree("a"), true) shouldBe Tree.empty
      modifyTree(0, f4, Tree("a", Tree("b")), true) shouldBe Tree("a")
      modifyTree(1, f4, Tree("a", Tree("b")), true) shouldBe Tree.empty
      modifyTree(2, f4, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree.empty
      modifyTree(1, f4, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("c"))
      modifyTree(0, f4, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree("a", Tree("b"))

      def previous(s: String): String = s match {
        case "b" => "a"
        case "c" => "b"
        case "d" => "c"
        case _   => "z"
      }

      val f5: Tree[String] => Tree[String] = t => Tree(previous(t.head), Tree("x"), Tree("y"))
      modifyTree(0, f5, Tree("a"), true) shouldBe Tree("z", Tree("x"), Tree("y"))
      modifyTree(0, f5, Tree("a", Tree("b")), true) shouldBe Tree("a", Tree("a", Tree("x"), Tree("y")))
      modifyTree(0, f5, Tree("a", Tree("b"), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("x"), Tree("y"))
      )
      modifyTree(0, f5, Tree("a", Tree("b", Tree("x"), Tree("z")), Tree("c")), true) shouldBe Tree(
        "a",
        Tree("b", Tree("x"), Tree("z"), Tree("y"))
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
