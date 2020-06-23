package com.github.arturopala.tree

import org.scalameter.api._

object TreeBenchmark extends Bench.LocalTime {

  val sizes: Gen[Int] = Gen.exponential("size")(100, 1000000, 10)

  // Generates a structure of a tree given a size and numberOfChildren constraints
  def genTreeStructure(size: Int, minNumberOfChildren: Int, maxNumberOfChildren: Int): Array[Int] = {

    val array: Array[Int] = new Array(size)
    var position: Int = size - 1
    var counter: Int = size - 1

    while (position >= 0) {

      val numberOfChildren =
        if (minNumberOfChildren == 0) size - 1
        else Math.min(counter, Math.max(minNumberOfChildren, (position + 1) % maxNumberOfChildren))

      array(position) = numberOfChildren
      position = position - 1
      counter = counter - numberOfChildren
    }

    array
  }

  type TreeArraysGen = Gen[(Array[Int], Array[String])]

  def treeStructureAndValues(minSubtreeWidth: Int, maxSubtreeWidth: Int): TreeArraysGen =
    for {
      size <- sizes
    } yield {
      (genTreeStructure(size, minSubtreeWidth, maxSubtreeWidth), (0 until size).map(_.toString).toArray)
    }

  val ultraWideTreesInput: TreeArraysGen = treeStructureAndValues(10, 100)
  val wideTreesInput: TreeArraysGen = treeStructureAndValues(1, 10)
  val narrowTreesInput: TreeArraysGen = treeStructureAndValues(1, 3)
  val binaryTreesInput: TreeArraysGen = treeStructureAndValues(2, 2)
  val singleBranchTreesInput: TreeArraysGen = treeStructureAndValues(1, 1)
  val singleNodeTreesInput: TreeArraysGen = treeStructureAndValues(0, 0)

  performance of "TreeBuilder" in {
    measure method "fromArrays for an ultra-wide trees" in {
      using(ultraWideTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromArrays for a wide trees" in {
      using(wideTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromArrays for a narrow trees" in {
      using(narrowTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromArrays for a binary trees" in {
      using(binaryTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromArrays for a single branch tree (max-deep)" in {
      using(singleBranchTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }

    measure method "fromArrays for a single node tree (max-wide)" in {
      using(singleNodeTreesInput) in {
        case (structure, values) =>
          TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty)
      }
    }
  }

  type TreeGen = Gen[(Int, Tree[String])]

  def buildTree(arrays: (Array[Int], Array[String])): (Int, Tree[String]) = arrays match {
    case (structure, values) =>
      (structure.length, TreeBuilder.fromIterables(structure, values, None).headOption.getOrElse(Tree.empty))
  }

  val ultraWideTrees: TreeGen = ultraWideTreesInput.map(buildTree)
  val wideTrees: TreeGen = wideTreesInput.map(buildTree)
  val narrowTrees: TreeGen = narrowTreesInput.map(buildTree)
  val binaryTrees: TreeGen = binaryTreesInput.map(buildTree)
  val singleBranchTrees: TreeGen = singleBranchTreesInput.map(buildTree)
  val singleNodeTrees: TreeGen = singleNodeTreesInput.map(buildTree)

  val listGen: Gen[List[String]] = sizes.map(size => (0 until size).map(_.toString).toList)

  performance of "Tree size and leafsSize" in {
    measure method "for a wide trees" in {
      using(wideTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.width
      }
    }

    measure method "for a narrow trees" in {
      using(narrowTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.width
      }
    }

    measure method "for a binary trees" in {
      using(binaryTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.width
      }
    }

    measure method "for a single branch trees" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          assert(tree.size == size)
          tree.width
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
    measure method "for an ultra-wide trees" in {
      using(ultraWideTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }

    measure method "for a wide trees" in {
      using(wideTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }

    measure method "for a narrow trees" in {
      using(narrowTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }

    measure method "for a binary trees" in {
      using(binaryTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }

    measure method "for a single branch trees (max-deep)" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }

    measure method "for a single node trees (max-wide)" in {
      using(singleBranchTrees) in {
        case (size, tree) =>
          tree.size
          tree.width
          tree.map(_ + "_")
      }
    }
  }
}
