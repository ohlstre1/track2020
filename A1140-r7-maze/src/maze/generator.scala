/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package maze

object generator {
  import Maze.Direction._
  import scala.collection.mutable.Buffer

  /**
   * Generate a (pseudo-random) maze of given dimensions.
   * The maze must not have any cycles and all the cells must be reachable
   * from the entry cell at (0,0).
   */
   def closeDir(cell : (Int, Int),rows: Int, columns: Int) : Buffer[Direction] = {
      val dir = Buffer[Direction]()
      if(cell._1 + 1 < rows) {
        dir += Up 
        }
      if(cell._2 + 1 < columns) {
        dir += Right
      }
      if(cell._1 - 1 >= 0) {
        dir += Down
        }
      if(cell._2 - 1 >= 0) {
        dir += Left
      }
      dir
    }
    
def closeCellDirection(cell : (Int, Int), dir : Direction) : (Int, Int) = {
      dir match {
        case Left  => (cell._1, cell._2 - 1)
        case Right => (cell._1, cell._2 + 1)
        case Down  => (cell._1 - 1, cell._2)
        case Up    => (cell._1 + 1, cell._2)
      }
    }
  
  def generate(rows: Int, columns: Int, seed: Int = System.nanoTime.toInt): Maze = {
    val rand = new scala.util.Random(seed)
    val maze = new Maze(rows, columns)
    val stack = scala.collection.mutable.Stack[(Int, Int)]()
    val visited = scala.collection.mutable.HashSet[(Int, Int)]()
           
    // Helper method for getting the cell next to a specified cell in a certain direction.
    
    
    
    stack.push((rand.nextInt(rows), rand.nextInt(columns)))
    while(stack.nonEmpty) {
      val now = stack.head
      if(!visited(now)) visited += now
      
      var randomDir = rand.shuffle(closeDir(now,rows,columns))
      var hasDir = false
      
      while(!hasDir && randomDir.nonEmpty) {
        if(!visited.contains(closeCellDirection(now, randomDir.head))) {
          maze.breakWall(now._1, now._2, randomDir.head)
          hasDir = true
          stack.push(closeCellDirection(now, randomDir.head))
        } else {
          randomDir = randomDir.tail
        }
      }
      
      if(!hasDir) stack.pop
    }
    maze
  }
}
