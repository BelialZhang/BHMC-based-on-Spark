package ga
//import org.apache.spark.broadcast.Broadcast
import scala.math
import scala.util._
import scala.io.Source
//#define RAND_MAX 0x7fff

object Initialization {
  def initialChromosome(times: Int,atomN: Int,rate: Int): Chromosome = {
    //按比例的初始化
    val N = 4*atomN
    val order = atomN
    var genes = new Array[Double](N)
    //val gen = new Array[Int](N)
    val r0 = 2.75
    var note = new Array[Int](order)
    var temp = 0
    var point = 0 
    var Energy = 0.0
    
    for(i <- 0 to order-1){
          note(i) = i
        }
    //产生01初始
    for(i <- 0 to order-1){
        point = Random.nextInt(order-i)
        temp = note(i)
        note(i) = note(point+i)
        note(point+i) = temp
        /***当前rate为Co原子的数量*****/
        if( note(i) < rate ){
          genes(i) = 0.0
        }
        else genes(i) = 1.0
      }
    
     //产生结构初始 引入初始构型
     if(times < 6)
    {
      for(i <- order to N-1){
        genes(i) = (Random.nextDouble()-0.5)*r0*math.pow(order,1.0/3.0)//通过公式在正方体内部初始化。
        //println(genes(i))
      }  
      Energy = Tool.localEnergy(genes, order)
      println("随机初始能量:" + Energy)
    }
    else if(times <14){
      
       Tool.coodFile(genes,atomN,1)//读入单金属最稳定的结构
       Energy = Tool.localEnergy(genes, order)
       println("Co初始能量:" + Energy)
     }
    else{
       Tool.coodFile(genes,atomN,0)//读入另单金属最稳定的结构
       Energy = Tool.localEnergy(genes, order)
       println("Pt初始能量:" + Energy)
    }
//    {
     //无初始构型
//    for(i <- order to N-1){
//        genes(i) = (Random.nextDouble()-0.5)*r0*math.pow(order,1.0/3.0)//通过公式在正方体内部初始化。
//        //Tool.coodFile(genes,atomN,0)//Co
//        //Tool.coodFile(genes,atomN,1)//Pt
//      }  
//      Energy = Tool.localEnergy(genes, order)
//      println("inti_energy:" + Energy)
//    }
    var newChrm = new Chromosome(genes)
    newChrm.fitness = Energy  
    newChrm
  }

  def initialPopulation(popSize: Int, atomN: Int,rate: Int, numPartitions: Int): Seq[(Int, Chromosome)] = {

    for (i <- 0 until popSize) yield (i , initialChromosome(i,atomN,rate))//确保一个Partition里面有一个个体
  }
}
