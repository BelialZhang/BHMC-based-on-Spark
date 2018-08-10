import breeze.linalg.{squaredDistance,DenseVector,Vector}
import org.apache.spark.sql.SparkSession
object SparkKMeans {
  def parseVector(line: String):Vector[Double] = 
  {
    DenseVector(line.split(' ').map(_.toDouble))
  }
  
  def closestPoint(p:Vector[Double],centers:Array[Vector[Double]]):Int = 
  {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    
    for(i <- 0 until centers.length)
    {
      val tempDist = squaredDistance(p,centers(i))
      if(tempDist < closest)
      {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }
  
  def showWarning() 
  {
    System.err.println(
        """WARN: This is a naive implementation of KMeans Clustering and is given as an example!
        """)
  }
  
}