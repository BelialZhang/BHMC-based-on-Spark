import Array._
import scala.math._
import scala.util.control.Breaks._
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
object FitnessCaculator {
  def main(args: Array[String])
  {
    val atomn = 38
    val CoN = 19
    val PtN = atomn - CoN
    var genes = new Array[Double](4*atomn)
    val filename = "src/Co"+ CoN +"Pt"+PtN+"_new.txt"
//    val filename = "src/Co"+ CoN +"Pt"+PtN+"_1.txt"
    val dataCP = Source.fromFile(filename)//读入文件
    val linesCP = dataCP.getLines.toArray
    //导入数据
    for(i<-0 until 4*atomn)
    {
      genes(i) = linesCP(i).toDouble//使用读入到文件的数据对genes进行赋值
    }
    dataCP.close()
    var R = Array.ofDim[Double](atomn,atomn)
    //计算距离
    for(i <- 0 until atomn)
    {
      for(j <- 0 until atomn){
         R(i)(j) = math.sqrt((genes(i + atomn)-genes(j + atomn))*(genes(i + atomn)-genes(j + atomn)) + (genes(i + 2*atomn)-genes(j + 2*atomn))*(genes(i + 2*atomn)-genes(j + 2*atomn)) + (genes(i + 3*atomn)-genes(j + 3*atomn))*(genes(i + 3*atomn)-genes(j + 3*atomn)))
      }
    }
     
    {
    
    var Energy = 0.0
    val N = 4*atomn
    val order = atomn
   
        //文老师提供
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
    
  
    
	  var tempV=0.0
	  var tempP=0.0
    var PEN = new Array[Double](order)
    var VEN = new Array[Double](order)

    
    for(i <- 0 until atomn-1)
    {
      for(j <- i+1 until atomn)
      {
					if (genes(i)>0.5&&genes(j)>0.5)
					{

						tempV = A_1*math.exp(P_1*(R(i)(j)/r0_1 - 1))
						tempP = xi_1*xi_1*math.exp(2*q_1*(R(i)(j)/r0_1 - 1))
						
					}
					else if (genes(i)<0.5&&genes(j)<0.5)
					{
						tempV = A*math.exp(P*(R(i)(j)/r0 - 1))
						tempP = xi*xi*math.exp(2*q*(R(i)(j)/r0 - 1))
					}
					else
					{
						tempV = A_2*math.exp(P_2*(R(i)(j)/r0_2 - 1))
						tempP = xi_2 * xi_2 * math.exp(2 * q_2 * (R(i)(j) / r0_2 - 1))
					}
			
					VEN(i) += tempV
					VEN(j) += tempV
					PEN(i) += tempP
					PEN(j) += tempP
      }
		}
				
				for(i <- 0 to order-1){
					PEN(i) = math.sqrt(PEN(i))
					Energy += VEN(i) - PEN(i)
				}
				println("Energy = " + Energy)
    //Energy
  }
         
  }
}