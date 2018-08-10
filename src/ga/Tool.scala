package ga
import Array._
import scala.math._
import scala.util.control.Breaks._
import scala.io.Source

object Tool {
  def distance(genes: Array[Double],R1: Array[Array[Double]],atomn: Int): Unit = {//计算原子间距离函数
    var Energy = 0.0
    val N = 4*atomn
    val order1 = atomn
  //首先求解R矩阵
    for(i <- 0 to order1-1){
      for(j <- 0 to order1-1){
         R1(i)(j) = math.sqrt((genes(i + order1)-genes(j + order1))*(genes(i + order1)-genes(j + order1)) + (genes(i + 2*order1)-genes(j + 2*order1))*(genes(i + 2*order1)-genes(j + 2*order1)) + (genes(i + 3*order1)-genes(j + 3*order1))*(genes(i + 3*order1)-genes(j + 3*order1)))
         //调用数学Math。sqrt
        }
      }
   // println("i=3,j=4的距离" + R1(3)(4))
    }
  
  //最外部的原子扰动到内部
   def distool(genes: Array[Double],R1: Array[Double],atomn: Int): Double = {
    var Energy = 0.0
    val N = 4*atomn
    var temp = 0.0
    var dcenter = new Array[Double](3)
    var tempR = new Array[Double](atomn)
   
    for(i <- 0 until atomn){
       dcenter(0) += genes(i + atomn) 
       dcenter(1) += genes(i + 2*atomn) 
       dcenter(2) += genes(i + 3*atomn) 
       
       dcenter(0) /= atomn
       dcenter(1) /= atomn
       dcenter(2) /= atomn
      }
     for(j <- 0 until atomn){
       tempR(j) = math.sqrt((dcenter(0)-genes(j + atomn))*(dcenter(0)-genes(j + atomn)) + (dcenter(1)-genes(j + 2*atomn))*(dcenter(1)-genes(j + 2*atomn)) + (dcenter(2)-genes(j + 3*atomn))*(dcenter(2)-genes(j + 3*atomn)))
       temp = tempR(0)
      }
     for(j <- 1 until atomn){
      if(temp<tempR(j))
        temp = tempR(j)
      }
    temp
    }
  
   //局部优化的具体实现
  def localEnergy(genes: Array[Double],atomn: Int): Double = {
    var Energy = 0.0
    val N = 4*atomn
    val order = atomn
    val e = 1E-6
    var Fmax = 0.0
    var alpha = 0.01
    var s = 1
    val smax = 3000
    var E0 = 0.0
    var E1 = 0.0
    //设置求解局部优化需要变量
    var PEN = new Array[Double](order)
    var FF = new Array[Double](3*order)
    var tempR = ofDim[Double](order,order)
    var tempgenes = new Array[Double](N)
    
  //首先求解R矩阵
    var R = ofDim[Double](order,order)
    distance(genes,R,atomn)
    E0 = Fitness.fitnessEnergy(genes,R,order)
    Fmax = Fitness.fitnessForce(genes,R,FF,order)
    
 
    //最速下降法的具体实现
    while(s<smax && alpha > e)
	  {
		
		  for(i <- 0 to order-1)
		  {
		    
			  tempgenes(i + order) = genes(i + order) + alpha * FF(i)/Fmax;
			  tempgenes(i + 2 * order) = genes(i + 2 * order) + alpha * FF(i + order)/Fmax;
			  tempgenes(i + 3 * order) = genes(i + 3 * order) + alpha * FF(i + 2 * order)/Fmax;
		  }

		  
		  distance(tempgenes,tempR,order);
		  E1 = Fitness.fitnessEnergy(genes,tempR,order);
		  if(E1 < E0)//需要根据能量值的大小去改变alpha
		  {
			  alpha = alpha * 1.1;
			  E0 = E1;
			
			  for(i <- 0 to order-1)
			  {
			  	genes(i+order) = tempgenes(i+order);
				  genes(i+2*order) = tempgenes(i+2*order);
			  	genes(i+3*order) = tempgenes(i+3*order);

			  }
		  

			  
		  	for(i <- 0 to order -1)
		  	{
		  	  for(j <- 0 to order -1) 
				  R(i)(j) = tempR(i)(j);
		  	}
			    Fmax = Fitness.fitnessForce(genes,R,FF,order);
		  }
		  else
		  {
		    alpha = alpha * 0.6;
		  }
			  
	    	s += 1;
	   }

     E0
    } 
  
  //读入文件具体实现
    def coodFile(genes: Array[Double],atomn: Int,check: Int): Unit = {
      
      if(check == 1){
        
        val filename = "src/Co"+ atomn +".txt"
//      val filename = "src/38new.txt"
        println(filename)
        val dataFe = Source.fromFile(filename)//读入文件
        println("读到了Co的数据")
       //写入数据到genes即可
        val lines = dataFe.getLines.toArray
        for(i<-atomn until 4*atomn)
        {
          genes(i) = lines(i-atomn).toDouble//使用读入到文件的数据对genes进行赋值
        }
        dataFe.close()
       
      }
      else{
        //val filename = "src/Pt" + atomn +".txt"
        val filename = "src/Pt"+ atomn +".txt"
        //val filename = "src/38new.txt"
        val dataPt = Source.fromFile(filename)
          //写入数据到genes即可
        val lines = dataPt.getLines.toArray
        println("读到了Pt的数据")
        for(i<-atomn until 4*atomn)
        {
          genes(i) = lines(i-atomn).toDouble//使用读入到文件的数据对genes进行赋值
          
        }
        dataPt.close()//关闭文件
      }
    }
  }
  