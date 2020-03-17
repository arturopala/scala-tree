package com.github.arturopala.tree

import org.scalameter.api._

import scala.annotation.tailrec

object TreeBuilderBenchmark extends Bench.LocalTime {

  val sizes = Gen.exponential("size")(100, 1000000, 10)

  def valueList(size: Int, p: Int): List[(Int, String)] = {

    @tailrec
    def list(maxSize: Int, currentSize: Int, result: List[(Int, String)], counter: Int): List[(Int, String)] =
      if (currentSize == maxSize) result
      else {
        val b = Math.min(counter, (currentSize + 1) % p)
        list(maxSize, currentSize + 1, (b, currentSize.toString) :: result, counter - b)
      }

    list(size, 0, Nil, size - 1)
  }

  val valueLists = for {
    size <- sizes
  } yield valueList(size, 10)

  performance of "Tree.Builder" in {
    measure method "fromValueList" in {
      using(valueLists) in { list =>
        Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }
  }
}
