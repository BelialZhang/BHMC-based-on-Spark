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
      //����ֲ������仯
      if(BestE == current.min()(Ordering[Double].on(x => x._2.fitness))._2.fitness)
      {
        count = count+1
        println("��ǰ������ͬ"+BestE)
      }
      else
      {
        count = 1
        println("������ͬ��BestE:"+BestE)
        BestE = current.min()(Ordering[Double].on(x => x._2.fitness))._2.fitness
        println("������"+BestE)
      } 
    //for (i <- 0 until maxgeneration) {//����������������ֹ
      println("��ǰ����������"+i)
      //���������ۼ�
      i += 1
      //var elitismRDD = sc.parallelize(current.top(elitismSize)(Ordering[Double].on(x => x._2.fitness)))
      //val sortedPop = current.sortBy(x => x._2.fitness)
     /*�����оɰ�û��*/
      val GBest = current.min()(Ordering[Double].on(x => x._2.fitness))
      println("the best energy is:" + GBest._2.fitness)
      
      
      //////*********************Map()ʵ��****************/////
//      ramake   
//        ����Ŷ�ʵ��
      val remakeRDD = current.map(pair => {//RDD�Ĵ�������Pair����ʽ
        val randGen = new Random()
       // if (randGen.nextDouble() < mutationRate) {//ÿ�����嶼Ҫ��������Ŷ�
          //evaluate fitness
        val pairtemp = pair
          println("before remake energy is :" + pair._2.fitness)
     
          var p = BHMCProcess.remake(pairtemp._2,atomN,rate)//����Ŷ��ľ���ʵ��
          p.fitness = Tool.localEnergy(p.genes, atomN)//����Tool�����local��������ֲ���С��������ֵ
          if(p.fitness < pair._2.fitness)//�Ƚ�ѡ��õ����ŵ�ʵ��ṹ
          {
            //for(i <- 0 until 4*atomN)
            pair._2.genes = p.genes
            pair._2.fitness = p.fitness
          }
          println("after remake energy is :" + pair._2.fitness)
          pair 
			 // }
      })
      
      //SphereCut�����Ŷ�
       val SphereCutRDD = remakeRDD.map(pair => {
        val pairtemp = pair
        val randGen = new Random()
        var child1g = new Array[Double](4*atomN)
        var child2g = new Array[Double](4*atomN)
        val child1 = new Chromosome(child1g)//��ʼ�Ӵ���Ⱥ
        val child2 = new Chromosome(child2g)//��ʼ�Ӵ���Ⱥ
        if (randGen.nextDouble < SphereCutRate) {//һ����������
          var penergy = BHMCProcess.SphereCut(pairtemp._2, GBest._2,child1,child2,atomN,rate)//�����Ŷ�����ʵ��
          if(penergy < pairtemp._2.fitness)//�Ƚ�ѡ����ŵĽ��
          {
            
            pair._2.genes = child1.genes
            pair._2.fitness = penergy
          }
        }

         println("after SphereCut energy is :" + pair._2.fitness)
         pair
      })  
      
       //�ṹ������������  
      val exchangeRDD = SphereCutRDD.map(pair => {
       val pairtemp = pair
       if(math.abs(pairtemp._2.fitness - GBest._2.fitness) >= 2.5)//������ֵ�ϲ�Ľ�����нṹ��������
          { 
            var tenergy = BHMCProcess.exchange(pairtemp._2,atomN,rate)//�ṹ���������ľ���ʵ��
            if(tenergy < pair._2.fitness)//�Ƚ�ѡ��õ����ŵ�ʵ��ṹ
            {
              pair._2.genes = pairtemp._2.genes
              pair._2.fitness = pairtemp._2.fitness
            }
          }
        println("after exchange energy is :" + pair._2.fitness)
        pair
      })
      
        


      val currentBest = exchangeRDD.min()(Ordering[Double].on(x => x._2.fitness))//����õ����к����ý��S
//       //////*********************GA�ֲ��Ż�ʵ��ʵ��****************/////
//      //val currentworst = remakeRDD.max()(Ordering[Double].on(x => x._2.fitness))
      val currentworst = exchangeRDD.max()(Ordering[Double].on(x => x._2.fitness))
      println("the best energy is:" + currentBest._2.fitness)
      println("the worst energy is:" + currentworst._2.fitness)
      val everendtime = System.nanoTime(): Double
      
      writer.println(i +"\t"+ currentBest._2.fitness + "\t" + (everendtime - startime)/1000000000.0)
     
      current.cache()     
    } 
    writer.close()
    println("�����ظ�����:"+count+" ��ǰ����:"+BestE)
    current
    
  }
  
  
   def Copy(first: Chromosome,initial: RDD[(Int, Chromosome)]): Unit = {
  
  }
  
}