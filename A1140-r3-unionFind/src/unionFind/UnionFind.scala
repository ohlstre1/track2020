/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package unionFind

object solver {
  /**
   * Given vertices {0,...,nofVertices-1} and weighted edges between them,
   * each edge of weight w between vertices v1 and v2 given as a triple
   * (v1, w, v2), find the largest weight W such that if we only consider
   * the edges of weight W or more, the graph stays connected (i.e., we can get
   * from any vertex to each other vertex).
   * Returns None if no such weight exists, i.e., if the graph is not connected
   * even if we consider all the edges given.
   * Provided that the union-find data structure is properly implemented and
   * includes either ranks or path compression (or both),
   * the running time of the algorithm is
   * O(nofVertices + |edges|*(log(|edges|) + log(nofVertices)))
   * on average.
   */
  def solve(nofVertices: Int, edges: Seq[(Int, Int, Int)]): Option[Int] = {
    require(nofVertices >= 1)
    edges.foreach({case (vertex1, weight, vertex2) =>
      require(0 <= vertex1 && vertex1 < nofVertices &&
              0 <= vertex2 && vertex2 < nofVertices &&
              weight >= 0)})
    // The sets of vertices 
    val sets = new UnionFind[Int]()
    var result = 0
    var i = 0
    val sortedEdges = edges.toArray.sortBy(-_._2)
    
    while(i < nofVertices){
      sets.makeSet(i)
      i +=1
    }
    
    i=0
    while(i < edges.size &&  sets.nofSets > 1 ){
      sets.union(sortedEdges(i)._1,sortedEdges(i)._3)
      result = sortedEdges(i)._2
      i += 1
    }
    
    
    if(i == edges.size) return None
    return Some(result)
  }
  
}


class UnionFind[E] {
  /* Insert your internal data structures here.
   * One possible choice is the following but
   * you are free to use your own choices as well:
//   */
    protected val parent = new scala.collection.mutable.HashMap[E, E]()
    protected val rank = new scala.collection.mutable.HashMap[E, Int]()
    protected var _nofSets = 0
    protected var _nofElements = 0
 

  /**
   * Introduce a new element in this disjoint sets data structure and
   * put it into the set consisting only of the element itself.
   * Does nothing if the element is already in the disjoint sets data structure.
   * Returns true if the element was inserted (i.e., was not already in
   * the data structure), false otherwise.
   * A constant-time operation (actually only on average if hash map
   * searches and insertions are used in the code).
   */
  def makeSet(element: E): Boolean = {
     if(!parent.contains(element)){
       parent += (element -> element)
       rank += (element -> 0)
       _nofSets += 1
       _nofElements += 1
       true
     }else false
  }
  
  /**
   * Get the representative element of the given element
   * in the current disjoint sets data structure.
   * Two elements are in the same set if and only if their representatives
   * are the same.
   * The representatives may change during union operations and
   * thus it is *not* safe to use previously calculated representatives
   * after an union operation has been performed.
   * Throws an exception if the element has not been introduced earlier
   * with makeSet.
   * An O(log n) operation on average,
   * where n is the number of elements in all the sets.
   */
  def findSet(element: E): E = {
    if(parent(element) == element) element
    else findSet(parent(element))
  }

  /**
   * Merge (i.e., make union of) the sets containing the elements
   * element1 and element2.
   * An O(log n) operation, where n is the number of elements in all the sets.
   */
  def union(element1: E, element2: E): Unit = {
    val parent1 = findSet(element1)
    val parent2 = findSet(element2)
    parent.put(element1, parent1)
    parent.put(element2, parent2)
    
    if(parent1 != parent2){
      if(rank(parent1) < rank(parent2)){
        parent.put(parent1, parent2)
        rank.put(parent1, rank(parent1) +1)
      }
      else{
        parent.put(parent2, parent1)
        rank.put(parent2, rank(parent2) +1)
      }
      _nofSets -= 1
    }
  }

  /** Get the number of elements in this disjoint sets data structure. */
  def nofElements: Int = {
    _nofElements
  }

  /** Get the number of sets in this disjoint sets data structure. */
  def nofSets: Int = {
    _nofSets
  }
}
