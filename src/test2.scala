import Array._
import scala.math._
import org.apache.spark._

object test2 {
  def prin(bdcast : broadcast.Broadcast[Array[Double]]) = {
    for(i <- 0 until bdcast.value.length)
    {
      println(bdcast.value(i))
      
     
     
    }
    
    //Co-Co
    var r0 = 2.502
	  var A = 0.106
	  var xi = 1.597
    var P = -10.870
	  var q = -2.360
	  //Pt-Pt
	  var r0_1 = 2.7747
	  var A_1 = 0.242
	  var xi_1 = 2.506
	  var P_1 = -11.140
	  var q_1 = -3.680
	  //Co-Pt
	  var r0_2 = 2.638
	  var A_2 = 0.182
	  var xi_2 = 2.146
	  var P_2 = -11.005
	  var q_2 = -3.020; 
    var tempP = 0.0
    var temp = 0.0
    for(i <- 0 until bdcast.value.length)
    {
      tempP= A_2*math.exp(P_2*(15/r0_2 - 1))
      temp = bdcast.value(11) * math.exp(bdcast.value(13) * (15 / bdcast.value(10) - 1))
      println("tempP="+tempP +"   temp="+temp)
    }
  }
}