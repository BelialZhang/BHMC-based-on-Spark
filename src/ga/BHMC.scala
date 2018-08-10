package ga
import java.io.{File, PrintWriter}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
//import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

class BHMC(val maxgeneration: Int,
                       val popSize: Int,
                       val atomN: Int,
                       val rate: Double,
                       val SphereCutRate: Double,
                       val mutationRate: Double,
                       val elitismRate: Double,
                       val tournamentSize: Int,
                       val numPartitions: Int,
                       val migrationPeriod: Int,
                       val migrationSize: Int) extends Serializable {



  def evolve(initial: RDD[(Int, Chromosome)],sc: SparkContext): RDD[(Int, Chromosome)] = {
    var current = initial
    var elitismSize = Math.ceil(elitismRate * popSize).toInt
    val selectSize = (popSize - elitismSize) / 2
    if ((popSize - elitismSize) % 2 != 0) elitismSize += 1
    val selectPercent = tournamentSize.toDouble / popSize * 2
    var fileName = "./result//atom"+atomN+"//Energy"+atomN+"_"+rate+"_1.txt"
    //val writer = new PrintWriter(new File("./result//Energy.txt" ))
    val writer = new PrintWriter(new File(fileName))
    //val writer1 = new PrintWriter(new File("/Users/jiqingshuang/Desktop/result//result.txt" ))
    val startime = System.nanoTime(): Double
    var BestE = current.min()(Ordering[Double].on(x => x._2.fitness))._2.fitness
    println(BestE)
    var count = 1
    var i = 0
    while(i<=maxgeneration)
    {
      //计算局部能量变化
      if(BestE == current.min()(Ordering[Double].on(x => x._2.fitness))._2.fitness)
      {
        count = count+1
        println("当前能量相同"+BestE)
      }
      else
      {
        count = 1
        println("能量不同，BestE:"+BestE)
        BestE = current.min()(Ordering[Double].on(x => x._2.fitness))._2.fitness
        println("新能量"+BestE)
      } 
    //for (i <- 0 until maxgeneration) {//设置最大迭代次数截止
      println("当前进化代数："+i)
      //进化代数累加
      i += 1
      //var elitismRDD = sc.parallelize(current.top(elitismSize)(Ordering[Double].on(x => x._2.fitness)))
      //val sortedPop = current.sortBy(x => x._2.fitness)
     /*下两行旧版没有*/
      val GBest = current.min()(Ordering[Double].on(x => x._2.fitness))
      println("the best energy is:" + GBest._2.fitness)
      
      
      //////*********************Map()实现****************/////
//      ramake   
//        随机扰动实现
      val remakeRDD = current.map(pair => {//RDD的处理是以Pair的形式
        val randGen = new Random()
       // if (randGen.nextDouble() < mutationRate) {//每个个体都要进行随机扰动
          //evaluate fitness
        val pairtemp = pair
          println("before remake energy is :" + pair._2.fitness)
     
          var p = BHMCProcess.remake(pairtemp._2,atomN,rate)//随机扰动的具体实现
          p.fitness = Tool.localEnergy(p.genes, atomN)//调用Tool里面的local函数计算局部极小化的能量值
          if(p.fitness < pair._2.fitness)//比较选择得到较优的实验结构
          {
            //for(i <- 0 until 4*atomN)
            pair._2.genes = p.genes
            pair._2.fitness = p.fitness
          }
          println("after remake energy is :" + pair._2.fitness)
          pair 
			 // }
      })
      
      //SphereCut球切扰动
       val SphereCutRDD = remakeRDD.map(pair => {
        val pairtemp = pair
        val randGen = new Random()
        var child1g = new Array[Double](4*atomN)
        var child2g = new Array[Double](4*atomN)
        val child1 = new Chromosome(child1g)//初始子代种群
        val child2 = new Chromosome(child2g)//初始子代种群
        if (randGen.nextDouble < SphereCutRate) {//一定概率球切
          var penergy = BHMCProcess.SphereCut(pairtemp._2, GBest._2,child1,child2,atomN,rate)//球切扰动具体实现
          if(penergy < pairtemp._2.fitness)//比较选择更优的结果
          {
            
            pair._2.genes = child1.genes
            pair._2.fitness = penergy
          }
        }

         println("after SphereCut energy is :" + pair._2.fitness)
         pair
      })  
      
       //结构重组修正操作  
      val exchangeRDD = SphereCutRDD.map(pair => {
       val pairtemp = pair
       if(math.abs(pairtemp._2.fitness - GBest._2.fitness) >= 2.5)//对能量值较差的结果进行结构重组修正
          { 
            var tenergy = BHMCProcess.exchange(pairtemp._2,atomN,rate)//结构重组修正的具体实现
            if(tenergy < pair._2.fitness)//比较选择得到较优的实验结构
            {
              pair._2.genes = pairtemp._2.genes
              pair._2.fitness = pairtemp._2.fitness
            }
          }
        println("after exchange energy is :" + pair._2.fitness)
        pair
      })
      
        


      val currentBest = exchangeRDD.min()(Ordering[Double].on(x => x._2.fitness))//计算得到球切后的最好结果S
//       //////*********************GA局部优化实现实现****************/////
//      //val currentworst = remakeRDD.max()(Ordering[Double].on(x => x._2.fitness))
      val currentworst = exchangeRDD.max()(Ordering[Double].on(x => x._2.fitness))
      println("the best energy is:" + currentBest._2.fitness)
      println("the worst energy is:" + currentworst._2.fitness)
      val everendtime = System.nanoTime(): Double
      
      writer.println(i +"\t"+ currentBest._2.fitness + "\t" + (everendtime - startime)/1000000000.0)
     
      current.cache()     
    } 
    writer.close()
    println("能量重复代数:"+count+" 当前能量:"+BestE)
    current
    
  }
  
  
   def Copy(first: Chromosome,initial: RDD[(Int, Chromosome)]): Unit = {
  
  }
  
}