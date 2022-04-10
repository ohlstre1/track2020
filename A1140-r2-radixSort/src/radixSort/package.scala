/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object radixSort {
  /**
   * Least-significant-digit-first radix sort for integer arrays.
   * Sorts the argument array in ascending order.
   * In the basic version (for 80 percent of the points),
   * you may assumes that all the integers are non-negative.
   * Interpret 'digit' as 'byte' (8 bits) here and
   * use bit-level operations such as
   * shifting (>> and <<) and masking (&) to extract 'digits'.
   */
  /*
  def countingSort(a: Array[Int], iterationNum: Int):Array[Int] = {
    val arr = new Array[Int](a.length)
    val count = new Array[Int](256)
    val length = a.length
    var i = 0
    val max = arr.max
    while( i < length){
      val element = a(i)>>(8*iterationNum) & 0xff
      count(element)+= 1 //räknar hur många gågnger en nummmer uppenbarar i intervallet 0-255
      i+=1
    } 

    
    val tempArr = new Array[Int](256)
    var j = 1
    while( j < 256){ 
      tempArr(j) = tempArr(j-1) + count(j-1) // Kumulativa platsen för nummror
      j += 1
    }
    
    i = 0
    var retu = new Array[Int](a.length)
    while(i < length){
      //j = (a(i)>>(8*iterationNum) ) & 0xff
      retu(tempArr((a(i)>>(8*iterationNum) ) & 0xff)) = a(i) 
      tempArr((a(i)>>(8*iterationNum) ) & 0xff) += 1
      i+=1
    }
    if(iterationNum<4){
         countingSort(retu, iterationNum +1)
    }else return retu
   
  } */
  /*
  def lsdfRadixSort(arr: Array[Int]): Unit = {
    val length = arr.length
    if(length <= 1) return
    
    def inner(A :Array[Int], shift_counter:Int) : Array[Int] = {
      val count = new Array[Int](256)
      var count_i = 0
      while(count_i < length) {
        val index = (A(count_i) >> shift_counter * 8) & 0xff
        count_i += 1
        count(index) += 1
      }
      count_i = 0
      var count_j = 1
      
      val temp_arr = new Array[Int](256)
      while(count_j > 256){
        temp_arr(count_j) = temp_arr(count_j - 1) + count(count_j-1)
        count_j += 1
      }
      val ret = new Array[Int](length)
      while( count_i < length){
        ret(temp_arr((A(count_i) >> shift_counter * 8) & 0xff)) = A(count_i)
        temp_arr((A(count_i) >> shift_counter * 8) & 0xff) += 1
        count_i +=1
      }
      if(shift_counter==3){
        return ret
      }else inner(ret, shift_counter +1)
      
    }
    
    
    
    val sorted = inner(arr,0)
    
    var i = 0
    while( i< arr.length){
      arr(i) = sorted(arr.indices(i))
      i+=1
    }
    
    
      
      
  }
  */
  
  
  def lsdRadixSort(a: Array[Int]): Unit = {
    val N = a.length
    if(N <= 1) return
    val comparer = 255
    var round = 0
    
    while (round < 4) {
      var skipRound = 0
      var index = 0
      
      while (index < N) {
        if(((a(index) >> round*8) ^ comparer) == comparer) {
          skipRound += 1
        }
        index += 1
      }
      
      if(skipRound != N) {
        var hits = Array.fill(256)(0)
        var j = 0
        while (j < N) {
          var temp = (a(j) >> round*8) & comparer
          hits(temp) = hits(temp) + 1
          j += 1
        }
        
        hits(0) = hits(0) - 1
        var hitsI = 1
        while(hitsI < 256) {
          hits(hitsI) = hits(hitsI) + hits(hitsI-1)
          hitsI += 1
        }
        
        var newArray = Array.fill(N)(0)
        var x = N-1
        while (x >= 0) {
          var temp = (a(x) >> round*8) & comparer
          var tempI = hits(temp)
          hits(temp) = hits(temp) - 1
          newArray(tempI) = a(x)
          x -= 1
        }
        
        var copyI = 0
        while (copyI < N) {
          a(copyI) = newArray(copyI)
          copyI += 1
        }
      }
      round += 1
    }
  }
  
    
}
