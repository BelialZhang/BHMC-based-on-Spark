package ga
import Array._
import scala.math._

object Fitness {
  var single = 3
  def fitnessEnergy(genes: Array[Double],R: Array[Array[Double]],atomn: Int): Double = {
    
    var Energy = 0.0
    val N = 4*atomn
    val order = atomn
   //求解势能函数(二合金)
//     //Co-Co
//	  var r0 = 2.50
//	  var A = 0.189
//	  var xi = 1.907
//    var P = -8.80
//	  var q = -2.96
//	  //Pt-Pt
//	  var r0_1 = 2.76
//	  var A_1 = 0.242
//	  var xi_1 = 2.506
//	  var P_1 = -11.14
//	  var q_1 = -3.68 
//	  //Co-Pt
//	  var r0_2 = 2.63
//	  var A_2 = 0.245
//	  var xi_2 = 2.386
//	  var P_2 = -9.97
//	  var q_2 = -3.32; 
    
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
    
    if(single == 1)//Co单金
    {
//      r0 = 2.50
//	    A = 0.189
//	    xi = 1.907
//	    P = -8.80
//	    q = -2.96
//	   
//	    r0_1 = 2.50
//	    A_1 = 0.189
//	    xi_1 = 1.907
//	    P_1 = -8.80
//	    q_1 = -2.96
//	   
//	    r0_2 = 2.50
//	    A_2 = 0.189
//	    xi_2 = 1.907
//	    P_2 = -8.80
//	    q_2 = -2.96;
//       r0 = 2.50
//	    A = 0.0950
//	    xi = 1.4880
//	    P = -11.604
//	    q = -2.286
//	     r0_1 = 2.50
//	    A_1 = 0.0950
//	    xi_1 = 1.4880
//	    P_1 = -11.604
//	    q_1 = -2.286
//	     r0_2 = 2.50
//	    A_2 = 0.0950
//	    xi_2 = 1.4880
//	    P_2 = -11.604
//	    q_2 = -2.286
	    
	      r0 = 2.502
	    A = 0.106
	    xi = 1.597
	    P = -10.870
	    q = -2.360
	    r0_1 = 2.502
	    A_1 = 0.106
	    xi_1 = 1.597
	    P_1 = -10.870
	    q_1 = -2.360
	    r0_2 = 2.502
	    A_2 = 0.106
	    xi_2 = 1.597
	    P_2 = -10.870
	    q_2 = -2.360
    }
    else if(single == 2)//Pt单金
    {
      r0 = 2.7747
	    A = 0.242
	    xi = 2.506
	    P = -11.14
	    q = -3.68 
	   
	    r0_1 = 2.7747
	    A_1 = 0.242
	    xi_1 = 2.506
	    P_1 = -11.14
	    q_1 = -3.68 
	   
	    r0_2 = 2.7747
	    A_2 = 0.242
	    xi_2 = 2.506
	    P_2 = -11.14
	    q_2 = -3.68 ;
    }
    
	  var tempV=0.0
	  var tempP=0.0
    var PEN = new Array[Double](order)
    var VEN = new Array[Double](order)
    
    var ji = 0.0
    var qi = 0.0
    var sh = 0.0
    var li = 0.0
    var go = 0.0
    var le = 0.0
   // var li = 0.0
    
    for(i <- 0 to order-2)
    {
      for(j <- i+1 to order-1)
      {
					if (genes(i)>0.5&&genes(j)>0.5)
					{
//					  ji = distanR(i)(j)/r0_1 - 1
//					  
//					  qi = math.exp(P_1*ji)
//					  sh = A_1*qi
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
//					  ji = distanR(i)(j)/r0_2 - 1
//					  li = ji*P_2
//					  qi = math.exp(li)
//					  sh = A_2*qi
//					  tempV = sh
//					  
//					  go = math.exp(2 * q_2 * ji)
//					  tempP = xi_2 * xi_2 * go
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
				//println("Energy = " + Energy)
    Energy
  }

   def fitnessForce(genes: Array[Double],R: Array[Array[Double]],FF: Array[Double],atomn: Int): Double = {
    var Energy = 0.0
    val N = 4*atomn
    val order = atomn
    val e = 1E-10

	  //求解势能函数(二合金)
//    //Co-Co
//	  var r0 = 2.50
//	  var A = 0.189
//	  var xi = 1.907
//	  var P = -8.80
//	  var q = -2.96
//	  //Pt-Pt
//	  var r0_1 = 2.76
//	  var A_1 = 0.242
//	  var xi_1 = 2.506
//	  var P_1 = -11.14
//	  var q_1 = -3.68 
//	  //Co-Pt
//	  var r0_2 = 2.63
//	  var A_2 = 0.245
//	  var xi_2 = 2.386
//	  var P_2 = -9.97
//	  var q_2 = -3.32; 
        
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
    
    if(single == 1)//Co单金
    {
//      r0 = 2.50
//	    A = 0.189
//	    xi = 1.907
//	    P = -8.80
//	    q = -2.96
//	   
//	    r0_1 = 2.50
//	    A_1 = 0.189
//	    xi_1 = 1.907
//	    P_1 = -8.80
//	    q_1 = -2.96
//	   
//	    r0_2 = 2.50
//	    A_2 = 0.189
//	    xi_2 = 1.907
//	    P_2 = -8.80
//	    q_2 = -2.96;
//      r0 = 2.50
//	    A = 0.0950
//	    xi = 1.4880
//	    P = -11.604
//	    q = -2.286
//	     r0_1 = 2.50
//	    A_1 = 0.0950
//	    xi_1 = 1.4880
//	    P_1 = -11.604
//	    q_1 = -2.286
//	     r0_2 = 2.50
//	    A_2 = 0.0950
//	    xi_2 = 1.4880
//	    P_2 = -11.604
//	    q_2 = -2.286
	    r0 = 2.502
	    A = 0.106
	    xi = 1.597
	    P = -10.870
	    q = -2.360
	    r0_1 = 2.502
	    A_1 = 0.106
	    xi_1 = 1.597
	    P_1 = -10.870
	    q_1 = -2.360
	    r0_2 = 2.502
	    A_2 = 0.106
	    xi_2 = 1.597
	    P_2 = -10.870
	    q_2 = -2.360
    }
    else if(single == 2)//Pt单金
    {
      r0 = 2.7747
	    A = 0.242
	    xi = 2.506
	    P = -11.14
	    q = -3.68 
	    
	    r0_1 = 2.7747
	    A_1 = 0.242
	    xi_1 = 2.506
	    P_1 = -11.14
	    q_1 = -3.68 
	   
	    r0_2 = 2.7747
	    A_2 = 0.242
	    xi_2 = 2.506
	    P_2 = -11.14
	    q_2 = -3.68 ;
    }
    
	  var tempdV=0.0
	  var tempP=0.0
	  var tempdP=0.0
    var PEN = new Array[Double](order)
    var VEN = new Array[Double](order)
    var Fk =0.0
    var Fmax = 0.0
    var ji = 0.0
    var qi = 0.0
    var sh = 0.0
    var li = 0.0
    var go = 0.0
    var le = 0.0
    for(i <- 0 to 3 * order - 1)
    {
      FF(i) = 0.0
    }
	  
	  for(i <- 0 to order-2)
    {
      for(j <- i+1 to order-1)
      {
					if (genes(i)>0.5&&genes(j)>0.5)
					{

						tempP = xi_1*xi_1*math.exp(2*q_1*(R(i)(j)/r0_1 - 1))
						
					}
					else if (genes(i)<0.5&&genes(j)<0.5)
					{
						tempP = xi*xi*math.exp(2*q*(R(i)(j)/r0 - 1))
					}
					else
					{
						tempP = xi_2 * xi_2 * math.exp(2 * q_2 * (R(i)(j) / r0_2 - 1))
					}
			  PEN(i) += tempP
			  PEN(j) += tempP
      }
    }
	  
	   for(i <- 0 to order - 1)
    {
	     if(PEN(i) == 0)
          PEN(i) = 0
	     else 
	        PEN(i) = 1/(math.sqrt(PEN(i)))
    }
	  
    
    for(i <- 0 to order-2)
    {
      for(j <- i+1 to order-1)
      {
					if (genes(i)>0.5&&genes(j)>0.5)
					{

						tempdV = A_1*P_1*exp(P_1*(R(i)(j)/r0_1 - 1))/r0_1
						tempdP = xi_1*xi_1*q_1*exp(2*q_1*(R(i)(j)/r0_1 - 1))/r0_1
						
					}
					else if (genes(i)<0.5&&genes(j)<0.5)
					{
						tempdV = A*P*exp(P*(R(i)(j)/r0 - 1))/r0
						tempdP = xi*xi*q*exp(2*q*(R(i)(j)/r0 - 1))/r0;
					}
					else
					{
						tempdV = A_2*P_2*exp(P_2*(R(i)(j)/r0_2 - 1))/r0_2
						tempdP = xi_2*xi_2*q_2*exp(2*q_2*(R(i)(j)/r0_2 - 1))/r0_2;
					}
					
          tempdP = (PEN(i) + PEN(j)) * tempdP
          Fk = -tempdV + tempdP/2
          FF(i)=FF(i)+Fk*(genes(i+order)-genes(j+order))/R(i)(j)
          FF(j)=FF(j)-Fk*(genes(i+order)-genes(j+order))/R(i)(j)
          FF(i+order)=FF(i+order)+Fk*(genes(i+2*order)-genes(j+2*order))/R(i)(j)
          FF(j+order)=FF(j+order)-Fk*(genes(i+2*order)-genes(j+2*order))/R(i)(j)
          FF(i+2*order)=FF(i+2*order)+Fk*(genes(i+3*order)-genes(j+3*order))/R(i)(j)
          FF(j+2*order)=FF(j+2*order)-Fk*(genes(i+3*order)-genes(j+3*order))/R(i)(j)//+-号搞错了我去去
      }
		}
				
				for(i <- 0 to 3*order-1){
				  if(FF(i)>Fmax)
				    Fmax = FF(i)
				}
				//println("Fmax = " + Fmax)
    Fmax
  }
}