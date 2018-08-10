package ga

import org.apache.spark.Partitioner

class RandomPartitioner(numParts: Int) extends Partitioner{
  override def numPartitions: Int = numParts
  override def getPartition(key: Any): Int = {
    scala.util.Random.nextInt(numPartitions)
  }
  override def equals(other: Any): Boolean = other match {
    case r: RandomPartitioner =>
      r.numPartitions == numPartitions
    case _ =>
      false
  }
  override def hashCode: Int = numPartitions
}
