/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object binarySearch {
  /**
   * The classic recursive binary serach, given here as a reference.
   * Returns true if and only if 'e' appears in the ordered array 'data'.
   */
  def search[A](data: IndexedSeq[A], e: A)(implicit ord: Ordering[A]): Boolean = {
    def inner(from: Int, to: Int): Boolean = {
      if(from <= to) {
        val mid = from + (to - from) / 2
        val cmp = ord.compare(e, data(mid))
        if(cmp == 0) true                   // e == data(mid)
        else if(cmp < 0) inner(from, mid-1) // e  < data(mid)
        else inner(mid+1, to)               // e  > data(mid)
      } else
        false
    }
    inner(0, data.length-1)
  }

  /*
   * Returns the smallest index i in the ordered array data
   * such that low <= data(i), or
   * None if all the elements in data are smaller than low.
   * As in the search method above, use ord.compare(x,y) to compare x and y.
   * Should only perform log2(n)+1 comparisons,
   * where n is the number of elements in data and
   * the logarithm is rounded up to the next integer.
   */
  def searchLow[A](data: IndexedSeq[A], low: A)(implicit ord: Ordering[A]): Option[Int] = {
    // Use of recursion is recommended but
    // you can also make an iterative version if you wish
    def inner(from: Int, to: Int): Option[Int] = {
        val mid = from + (to - from) / 2
      if(from <= to){
        val cmp = ord.compare(low, data(mid))
        if(cmp == 0 || cmp < 0 ) {
          val temp = inner(from, mid-1) 
          if(temp.isEmpty){
            Some(mid) 
          }else
            temp
        }
        else inner(mid+1, to)               // low  > data(mid)
      }else{
         None
      }
       
   }   
      //if(ord.compare(low, data(data.length-1)) > 0) return  None
       (inner(0, data.length-1))
       
  }
  
  
  
  
  
  
  
}
  
  
  
  /*
    def findPos(): Option[Int] ={
      var l = 0
      var h = 1
      var temp = data(0)
      
      while (ord.compare(temp, low) < 0){
        l= h
        h = 2*h
        temp = data(h)
      }
      */
  
