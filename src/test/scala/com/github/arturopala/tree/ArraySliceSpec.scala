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

import com.github.arturopala.tree.util.Slice

class ArraySliceSpec extends AnyWordSpecCompat {

  "Slice" should {
    "wrap a whole array" in {
      Slice.of(Array("a", "b", "c", "d", "e")).toArray shouldBe Array("a", "b", "c", "d", "e")
      Slice.of(Array.empty[String]).toArray shouldBe Array.empty[String]
    }

    "wrap a slice of an array" in {
      Slice.of(Array("a", "b", "c", "d", "e"), 2, 5).toArray shouldBe Array("c", "d", "e")
      Slice.of(Array("a", "b", "c", "d", "e"), 4, 4).toArray shouldBe Array.empty[String]
    }

    "have isEmpty" in {
      Slice.of(Array("a", "b", "c", "d", "e")).isEmpty shouldBe false
      Slice.of(Array("a", "b", "c", "d", "e"), 2, 4).isEmpty shouldBe false
      Slice.of(Array("a", "b", "c", "d", "e"), 2, 2).isEmpty shouldBe true
      Slice.of(Array.empty[String], 0, 0).isEmpty shouldBe true
    }

    "iterate over slice of values" in {
      Slice.of(Array("a", "b", "c", "d", "e")).iterator.toList shouldBe List("a", "b", "c", "d", "e")
      Slice.of(Array("a", "b", "c", "d", "e"), 1, 5).iterator.toList shouldBe List("b", "c", "d", "e")
      Slice.of(Array("a", "b", "c", "d", "e"), 1, 4).iterator.toList shouldBe List("b", "c", "d")
      Slice.of(Array("a", "b", "c", "d", "e"), 2, 3).iterator.toList shouldBe List("c")
    }

    "reverse-iterate over slice of values" in {
      Slice
        .of(Array("a", "b", "c", "d", "e"))
        .reverseIterator
        .toList shouldBe List("a", "b", "c", "d", "e").reverse
      Slice
        .of(Array("a", "b", "c", "d", "e"), 1, 5)
        .reverseIterator
        .toList shouldBe List("b", "c", "d", "e").reverse
      Slice.of(Array("a", "b", "c", "d", "e"), 1, 4).reverseIterator.toList shouldBe List("b", "c", "d").reverse
      Slice.of(Array("a", "b", "c", "d", "e"), 2, 3).reverseIterator.toList shouldBe List("c").reverse
      Slice.of(Array("a", "b", "c", "d", "e"), 0, 0).reverseIterator.toList shouldBe List()
      Slice.of(Array("a", "b", "c", "d", "e"), 5, 5).reverseIterator.toList shouldBe List()
    }

    "reverse-iterate with filter over slice of values" in {
      Slice.of(Array.empty[Int]).reverseIterator(_                       % 2 == 0).toList shouldBe Nil
      Slice.of(Array(1)).reverseIterator(_                               % 2 == 0).toList shouldBe Nil
      Slice.of(Array(1, 2)).reverseIterator(_                            % 2 != 0).toList shouldBe List(1)
      Slice.of(Array(1)).reverseIterator(_                               % 2 != 0).toList shouldBe List(1)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).reverseIterator(_       % 2 == 0).toList shouldBe List(8, 6, 4, 2)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 7).reverseIterator(_ % 2 == 0).toList shouldBe List(6, 4)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5).reverseIterator(_ % 2 == 0).toList shouldBe List(4)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 5).reverseIterator(_ % 2 == 0).toList shouldBe List(4)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 4, 5).reverseIterator(_ % 2 == 0).toList shouldBe Nil
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 0).reverseIterator(_ % 2 == 0).toList shouldBe Nil
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 5).reverseIterator(_ % 2 == 0).toList shouldBe Nil
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 9).reverseIterator(_ % 2 == 0).toList shouldBe List(8, 6, 4, 2)
    }

    "map slice of values" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).map(_ * 10).toArray shouldBe Array(10, 20, 30, 40, 50, 60, 70, 80, 90)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).map(_ * 10).toArray shouldBe Array(30, 40, 50, 60)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 3).map(_ * 10).toArray shouldBe Array(30)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 2).map(_ * 10).toArray shouldBe Array.empty[Int]
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 0).map(_ * 10).toArray shouldBe Array.empty[Int]
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 9, 9).map(_ * 10).toArray shouldBe Array.empty[Int]
    }

    "count values in slice matching predicate" in {
      Slice(0, 2, 0, 4, 5, 0, 7, 8, 0).count(_ == 0) shouldBe 4
      Slice(3, 4, 5).count(_ == 0) shouldBe 0
      Slice(0, 0, 0, 0, 0).count(_ == 0) shouldBe 5
      Slice(0).count(_ == 0) shouldBe 1
      Slice(1).count(_ == 0) shouldBe 0
      Slice[Int]().count(_ == 0) shouldBe 0
    }

    "top a value by an index" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).apply(3) shouldBe 4
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 9).apply(3) shouldBe 6
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 9).apply(3) shouldBe 9
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).apply(3) shouldBe 6
      an[IndexOutOfBoundsException] shouldBe thrownBy {
        Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).apply(4)
      }
      an[IndexOutOfBoundsException] shouldBe thrownBy {
        Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).apply(-1)
      }
    }

    "update a value within a slice" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).update(3, 12).apply(3) shouldBe 12
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).update(3, 12).apply(4) shouldBe 5
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 9).update(3, -13).apply(3) shouldBe -13
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 9).update(3, -13).apply(2) shouldBe 5
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 9).update(3, 0).apply(3) shouldBe 0
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).update(3, -1).apply(3) shouldBe -1
      an[IndexOutOfBoundsException] shouldBe thrownBy {
        Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).update(4, 0)
      }
      an[IndexOutOfBoundsException] shouldBe thrownBy {
        Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 6).update(-1, 0)
      }
    }

    "have a length" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).length shouldBe 9
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 0).length shouldBe 0
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 1).length shouldBe 1
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 8).length shouldBe 6
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 4, 5).length shouldBe 1
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 5).length shouldBe 0
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 9).length shouldBe 4
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 8, 9).length shouldBe 1
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 9, 9).length shouldBe 0
    }

    "have a slice" in {
      Slice[Int]().slice(-5, 10) shouldBe Slice[Int]()
      Slice(1, 2, 3).slice(-5, 10) shouldBe Slice(1, 2, 3)
      Slice(1, 2, 3, 4, 5, 6, 7, 8, 9).slice(-5, 5) shouldBe Slice(1, 2, 3, 4, 5)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 7).slice(-5, 5) shouldBe Slice(3, 4, 5, 6, 7)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5).slice(5, 8) shouldBe Slice[Int]()
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 5, 5).slice(1, 2) shouldBe Slice[Int]()
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 5).slice(1, 2) shouldBe Slice(5)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 9).slice(7, 12) shouldBe Slice(8, 9)
    }

    "have a drop" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).drop(0).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).drop(3).toList shouldBe List(4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).drop(8).toList shouldBe List(9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).drop(9).toList shouldBe Nil
      Slice.of(Array(1, 2, 3)).drop(3).toList shouldBe Nil
      Slice.of(Array(1, 2, 3)).drop(9).toList shouldBe Nil
      Slice.of(Array(1, 2)).drop(3).toList shouldBe Nil
      Slice.of(Array(1)).drop(3).toList shouldBe Nil
      Slice.of(Array.empty[Int]).drop(3).toList shouldBe Nil
    }

    "have a dropRight" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).dropRight(0).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).dropRight(3).toList shouldBe List(1, 2, 3, 4, 5, 6)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).dropRight(8).toList shouldBe List(1)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).dropRight(9).toList shouldBe Nil
      Slice.of(Array(1, 2, 3)).dropRight(3).toList shouldBe Nil
      Slice.of(Array(1, 2, 3)).dropRight(9).toList shouldBe Nil
      Slice.of(Array(1, 2)).dropRight(3).toList shouldBe Nil
      Slice.of(Array(1)).dropRight(3).toList shouldBe Nil
      Slice.of(Array.empty[Int]).dropRight(3).toList shouldBe Nil
    }

    "have a take" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(0).toList shouldBe Nil
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(3).toList shouldBe List(1, 2, 3)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(8).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(9).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3)).take(3).toList shouldBe List(1, 2, 3)
      Slice.of(Array(1, 2, 3)).take(9).toList shouldBe List(1, 2, 3)
      Slice.of(Array(1, 2)).take(3).toList shouldBe List(1, 2)
      Slice.of(Array(1)).take(3).toList shouldBe List(1)
      Slice.of(Array.empty[Int]).take(3).toList shouldBe Nil
    }

    "have a takeRight" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).takeRight(0).toList shouldBe Nil
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).takeRight(3).toList shouldBe List(7, 8, 9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).takeRight(8).toList shouldBe List(2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).takeRight(9).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(1, 2, 3)).takeRight(3).toList shouldBe List(1, 2, 3)
      Slice.of(Array(1, 2, 3)).takeRight(9).toList shouldBe List(1, 2, 3)
      Slice.of(Array(1, 2)).takeRight(3).toList shouldBe List(1, 2)
      Slice.of(Array(1)).takeRight(3).toList shouldBe List(1)
      Slice.of(Array.empty[Int]).takeRight(3).toList shouldBe Nil
    }

    "have a head" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).head shouldBe 1
      Slice.of(Array(4)).head shouldBe 4
      an[NoSuchElementException] shouldBe thrownBy(Slice.of(Array.empty[String]).head)
    }

    "have a headOption" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).headOption shouldBe Some(1)
      Slice.of(Array(4)).headOption shouldBe Some(4)
      Slice.of(Array.empty[String]).headOption shouldBe None
    }

    "have a tail" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).tail.toList shouldBe List(2, 3, 4, 5, 6, 7, 8, 9)
      Slice.of(Array(4)).tail.toList shouldBe Nil
      Slice.of(Array.empty[String]).tail.toList shouldBe Nil
    }

    "have a last" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).last shouldBe 9
      Slice.of(Array(4)).last shouldBe 4
      an[NoSuchElementException] shouldBe thrownBy(Slice.of(Array.empty[String]).head)
    }

    "have a lastOption" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).lastOption shouldBe Some(9)
      Slice.of(Array(4)).lastOption shouldBe Some(4)
      Slice.of(Array.empty[String]).lastOption shouldBe None
    }

    "have an init" in {
      Slice.of(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)).init.toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
      Slice.of(Array(4)).init.toList shouldBe Nil
      Slice.of(Array.empty[String]).init.toList shouldBe Nil
    }
  }

}
