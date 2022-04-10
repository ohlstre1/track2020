package object knapsack {
  /**
   * 
   * A recursive backtracking search algorithm that takes exponential time in
   * the number of items.
   * Given for reference and comparison purposes.
   */
  def solveRecursive(maxWeight: Int, items: Seq[Item]): (Int, Seq[Item]) = {
    // Check that all weights and values are positive
    items.foreach({case Item(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    /*
     * An inner function for performing the actual search
     */
    def search(currentMaxW: Int, remainingItems: List[Item]): (Int, List[Item]) = {
      if(remainingItems.isEmpty) (0, Nil)
      else {
        val item = remainingItems.head
        // Get the best value when the item is included
        val included =
          if(item.weight > currentMaxW) (0, Nil)
          else {
            val (subValue, subSol) = search(currentMaxW - item.weight, remainingItems.tail)
            (subValue + item.value, item :: subSol)
          }
        // Get the best value when the item is not included
        val excluded = search(currentMaxW, remainingItems.tail)
        if(included._1 > excluded._1) included else excluded
      }
    }
    // Do the actual recursive seach for the solution
    // A heuristic: choose heavy objects first
    search(maxWeight, items.sortBy(_.weight).reverse.toList)
  }

  /**
   * Given a maximum weight and a list of objects as (weight, value) pairs,
   * find the highest-value subset of the items that weighs at most maxWeight.
   * Uses dynamic programming and should be much better than the recursive
   * search approach for instances with many items but
   * relatively small maxWeight.
   */
  def solveDynProg(maxWeight: Int, items: Seq[Item]): (Int, Seq[Item]) = {
    // Check that all weights and values are positive
    items.foreach({case Item(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    ???
  }
}
