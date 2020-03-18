package com.github.arturopala.tree

import org.scalameter.api._

import scala.annotation.tailrec

object TreeBenchmark extends Bench.LocalTime {

  val sizes: Gen[Int] = Gen.exponential("size")(100, 1000000, 10)

  // Generates an input to create a tree for a given size and width constraints
  def valueList(size: Int, minSubtreeWidth: Int, maxSubtreeWidth: Int): List[(Int, String)] = {

    @tailrec
    def list(maxSize: Int, currentSize: Int, result: List[(Int, String)], counter: Int): List[(Int, String)] =
      if (currentSize == maxSize) result
      else {
        val subtreeWidth =
          if (minSubtreeWidth == 0) size - 1
          else Math.min(counter, Math.max(minSubtreeWidth, (currentSize + 1) % maxSubtreeWidth))
        list(maxSize, currentSize + 1, (subtreeWidth, currentSize.toString) :: result, counter - subtreeWidth)
      }

    list(size, 0, Nil, size - 1)
  }

  type ValueListGen = Gen[(Int, List[(Int, String)])]

  def valueLists(minSubtreeWidth: Int, maxSubtreeWidth: Int): ValueListGen =
    for {
      size <- sizes
    } yield (size, valueList(size, minSubtreeWidth, maxSubtreeWidth))

  val ultraWideTreesInput: ValueListGen = valueLists(10, 100)
  val wideTreesInput: ValueListGen = valueLists(1, 10)
  val narrowTreesInput: ValueListGen = valueLists(1, 3)
  val binaryTreesInput: ValueListGen = valueLists(2, 2)
  val singleBranchTreesInput: ValueListGen = valueLists(1, 1)
  val singleNodeTreesInput: ValueListGen = valueLists(0, 0)

  performance of "Tree.Builder" in {
    measure method "fromValueList for ultra-wide trees" in {
      using(ultraWideTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromValueList for wide trees" in {
      using(wideTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromValueList for narrow trees" in {
      using(narrowTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromValueList for binary trees" in {
      using(binaryTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromValueList for a single branch tree (max-deep)" in {
      using(singleBranchTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromValueList for a single node tree (max-wide)" in {
      using(singleNodeTreesInput) in {
        case (size, list) =>
          Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)
      }
    }
  }

  type TreeGen = Gen[(Int, Tree[String])]

  def buildTree(list: List[(Int, String)]): Tree[String] =
    Tree.Builder.fromValueList(list).headOption.getOrElse(Tree.empty)

  val ultraWideTrees: TreeGen = ultraWideTreesInput.map { case (size, list) => (size, buildTree(list)) }
  val wideTrees: TreeGen = wideTreesInput.map { case (size, list)           => (size, buildTree(list)) }
  val narrowTrees: TreeGen = narrowTreesInput.map { case (size, list)       => (size, buildTree(list)) }
  val binaryTrees: TreeGen = binaryTreesInput.map { case (size, list)       => (size, buildTree(list)) }
  val singleBranchTrees: TreeGen =
    singleBranchTreesInput.map { case (size, list)                            => (size, buildTree(list)) }
  val singleNodeTrees: TreeGen = singleNodeTreesInput.map { case (size, list) => (size, buildTree(list)) }

  val listGen: Gen[List[String]] = sizes.map(size => (0 until size).map(_.toString).toList)

  performance of "Tree size and leafsSize" in {
    measure method "for wide trees" in {
      using(wideTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.leafsSize
      }
    }

    measure method "for narrow trees" in {
      using(narrowTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.leafsSize
      }
    }

    measure method "binary trees" in {
      using(binaryTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.leafsSize
      }
    }

    measure method "single branch trees" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.leafsSize
      }
    }
  }

  performance of "List" in {
    measure method "map" in {
      using(listGen) in { list =>
        list.map(_ + "_")
      }
    }
  }

  performance of "Tree map" in {
    measure method "for ultre-wide trees" in {
      using(ultraWideTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }

    measure method "for wide trees" in {
      using(wideTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }

    measure method "for narrow trees" in {
      using(narrowTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }

    measure method "binary trees" in {
      using(binaryTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }

    measure method "single branch trees (max-deep)" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }

    measure method "single node trees (max-wide)" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          tree.size
          tree.leafsSize
          tree.map(_ + "_")
      }
    }
  }
}
