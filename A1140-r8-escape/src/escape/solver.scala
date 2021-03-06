/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

object solver {
  /**
   * Find a shortest-duration way from the world.startPosition to
   * a safe position in the world and return the path in form of
   * a sequence of (row,column) coordinates.
   * You can use the World.neighbours method for finding the valid
   * neighbour coordinates of a coordinate.
   */
  def solve(world: World): Seq[(Int, Int)] = {
    var seq = Seq[(Int, Int)]()
    val seen = Array.fill[Boolean](world.nofRows, world.nofColumns)(false)
    val que = scala.collection.mutable.PriorityQueue[((Int, Int), Int)]()(Ordering.by(-_._2))
    var duration = Array.fill[Int](world.nofRows, world.nofColumns)(Integer.MAX_VALUE)
    var pred = scala.collection.mutable.HashMap[(Int, Int), (Int, Int)]()
  
 
    que.enqueue((world.startPosition, 0))
    duration(world.startPosition._1)(world.startPosition._2) = 0
    
    var stop = false ; var qDequeue = que.dequeue
    val bool1 = !seen(qDequeue._1._1)(qDequeue._1._2)
    if(bool1){

      if(world.apply(qDequeue._1).duration == 0 && qDequeue._1 != world.startPosition){
          seq = seq.+:(qDequeue._1)
          stop = true
        
      }else{
          
          seen(qDequeue._1._1)(qDequeue._1._2) = true 
          for(n <- world.neighbours(qDequeue._1)){
          val bool2 = duration(qDequeue._1._1)(qDequeue._1._2) + world.apply(n).duration < duration(n._1)(n._2) 
          if(bool2){
              
            duration(n._1)(n._2) = duration(qDequeue._1._1)(qDequeue._1._2) + world.apply(n).duration
            pred(n) = qDequeue._1
            que.enqueue((n, duration(n._1)(n._2)))
            }
          }
        }
      }
   
    while(!stop){
      qDequeue = que.dequeue
      
      val bool1 = !seen(qDequeue._1._1)(qDequeue._1._2)
      if(bool1){
        if(world.apply(qDequeue._1).duration == 0 && qDequeue._1 != world.startPosition){
          seq = seq.+:(qDequeue._1)
          stop = true
        }else{
          seen(qDequeue._1._1)(qDequeue._1._2) = true 
          for(n <- world.neighbours(qDequeue._1)){
          val bool2 = duration(qDequeue._1._1)(qDequeue._1._2) + world.apply(n).duration < duration(n._1)(n._2) 
          if(bool2){
              duration(n._1)(n._2) = duration(qDequeue._1._1)(qDequeue._1._2) + world.apply(n).duration
              pred(n) = qDequeue._1
              que.enqueue((n, duration(n._1)(n._2)))
            }
          }
        }
      }
    }
 
    while(world.startPosition != seq.head){
      seq = seq.+:(pred(seq.head))
    }
 
    seq
  }

}
