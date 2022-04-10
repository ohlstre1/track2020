package object rgbStrings {
  /**
   * Count the number of all strings of length n over
   * the characterst 'r', 'g', and 'b'
   * in which it holds that
   * 'r' is never next to 'g' and vise versa.
   * For instance, when 'length' is 3, then the answer is 17 (
   * the strings are rrr, rrb, rbr, rbg, rbb, ggg, ggb, gbr, gbg, rbb,
   * brr, brb, bgg, bgb, bbr, bbg, and bbb ).
   * Note: the numbers can grow VERY large => use BigInt in all the computations.
   * Note: we are only interested in the number of such strings and one should NOT explicitly generate any of them. Thus your code should NOT contain any vars or vals of type String or Char.
   */
  
  
  def count(n: Int): BigInt = {
    require(n > 0)
    
    var x = BigInt (0) ; var y = BigInt (1) ; var z = BigInt (3); var w = BigInt (1)
    for (useless <- 1 to n-1) {
      var temp = 2*z + w
      w = z
      z = temp
      //dela
      temp = 2*y + x
      x = y
      y = temp
      
    }
    return 2*y + w
  
    
  }
}
