/* Authors: Markus Arlander and Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object rabinKarp {
  /**
   * A reference implementation of the naive substring search algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in time O(nm), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstringNaive(text: String, pattern: String): Int = {
    val n = text.size
    val m = pattern.size
    val end = n - m
    var i = 0
    while(i <= end) {
      var j = i
      var k = 0
      while(k < m && text(j) == pattern(k)) {
        j += 1
        k += 1
      }
      if(k == m)
        return i
      i += 1
    }
    -1
  }

  /**
   * Substring search with the Rabin-Karp algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in expected time O(n+m), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstring(text: String, pattern: String): Int = {
    
    val n = text.size
    val m = pattern.size
    if(m > n)
      return -1
 
      
    var j = 1 ; var i = m - 1 ; var hash = 0 
    while(i >= 0) {
      hash += pattern(i) * j
      i -= 1
      j = j * 101
    }
    
    j = 1 ; i = m - 1; var hashCurrent = 0 
    while(i >= 0) {
      hashCurrent += text(i) * j
      if(i != 0) j = 101 * j
      i -= 1
    }
 
    if (hashCurrent == hash && pattern == text.substring(0, m)) return 0
    i = 1
    while (i + m <= n) {
      hashCurrent =  text( i+m-1 ).toInt + ( hashCurrent-j* text( i-1 ).toInt )* 101 
      val returnsTrue = hashCurrent == hash && pattern == text.substring(i, i + m)
      if (returnsTrue) return i
      i += 1
    }
    
    
    return -1
  }
  
}
