import scala.util.matching.Regex
import org.apache.spark._
object test {
  def main(args: Array[String])
  {
    val conf = new SparkConf().setMaster("local[1]").setAppName("test")
    val sc = new SparkContext(conf)
    val conf1 = new SparkConf().setMaster("local[1]").setAppName("test1")
    val bdcast = sc.broadcast(Array(2.502,0.106,1.597,-10.870,-2.360,2.7747,0.242,2.506,-11.140,-3.680,2.638,0.182,2.146,-11.005,-3.0208))
    test2.prin(bdcast)
    //println(broadcast1.value)
    //var num = broadcast1.value
    //println(broadcast1.value(4)) 
    //var count = num(1)._2+2;
    //println(count)
  }
}


//public class Solution{
//  public List<String> find Strobogrammatic(int n)
//  {
//    
//  }
//}