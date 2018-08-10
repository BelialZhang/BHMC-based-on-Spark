import Array._
import scala.math._
import scala.util.control.Breaks._
import scala.io.Source
import java.io.{File, PrintWriter}
object SCount {
  def main(args: Array[String])
  {
    val atomn = 38
    val CoN = 34
    val PtN = atomn - CoN
    var Cgenes = new Array[Double](4*atomn)
    var Pgenes = new Array[Double](4*atomn)
    var CPgenes = new Array[Double](4*atomn)
    /*读取数据阵,
     * 		0		~	atomn		为原子类型0.0代表Co，1.0代表Pt
     * atomn	~	2*atomn	为x轴坐标
     * 2*atomn~	3*atomn	为y轴坐标
     * 3*atomn~	4*atomn 为z轴坐标
     * */
    //val filename1 = "src/55Cut.txt"
    val filename1 = "src/Co"+38+".txt"
    val filename2 = "src/Pt"+38+".txt"
    val filename3 = "src/3/38-"+ CoN +".txt"
    println("读入Co的数据")
    val dataCo = Source.fromFile(filename1)//读入文件
     val linesC = dataCo.getLines.toArray
    println("读入Pt的数据")
    val dataPt = Source.fromFile(filename2)//读入文件
    val linesP = dataPt.getLines.toArray
    println("读入CoPt的数据")
    val dataCP = Source.fromFile(filename3)//读入文件
    val linesCP = dataCP.getLines.toArray
    println("Data analysis")
    for(i<- 0 until 3*atomn)
    {
      Cgenes(i) = linesC(i).toDouble//使用读入到文件的数据对genes进行赋值
      Pgenes(i) = linesP(i).toDouble//使用读入到文件的数据对genes进行赋值
      
    }
    for(i<-0 until 4*atomn)
    {
      CPgenes(i) = linesCP(i).toDouble//使用读入到文件的数据对genes进行赋值
    }
    for(i<- 0 until atomn)
    {
      println(CPgenes(i))
    }
    dataCo.close()
    dataPt.close()
    dataCP.close()
    /*计算几何中心*/
    var Ccenter = new Array[Double](3)
    var Pcenter = new Array[Double](3)
    var CPcenter = new Array[Double](3)
    for(i<-0 until atomn)
    {
      Ccenter(0) += Cgenes(i)/atomn
      Ccenter(1) += Cgenes(i+atomn)/atomn
      Ccenter(2) += Cgenes(i+2*atomn)/atomn
      
      Pcenter(0) += Pgenes(i)/atomn
      Pcenter(1) += Pgenes(i+atomn)/atomn
      Pcenter(2) += Pgenes(i+2*atomn)/atomn
      
      CPcenter(0) += CPgenes(i+atomn)/atomn
      CPcenter(1) += CPgenes(i+2*atomn)/atomn
      CPcenter(2) += CPgenes(i+3*atomn)/atomn
    }
    /*计算相似度：S相似度，R几何距离*/
    var Sc = 0.0
    var Sp = 0.0
    var qc = 0.0
    var qp = 0.0
    var Rc = new Array[Double](atomn)
    var Rp = new Array[Double](atomn)
    var Rcp = new Array[Double](atomn)
    println("计算距离")
    for(i<-0 until atomn)
    {
      //println(i+"号计算")
      Rc(i) = math.sqrt((Cgenes(i)-Ccenter(0))*(Cgenes(i)-Ccenter(0)) +(Cgenes(i+atomn)-Ccenter(1))*(Cgenes(i+atomn)-Ccenter(1))+(Cgenes(i+2*atomn)-Ccenter(2))*(Cgenes(i+2*atomn)-Ccenter(2)))
      Rp(i) = math.sqrt((Pgenes(i)-Pcenter(0))*(Pgenes(i)-Pcenter(0))+(Pgenes(i+atomn)-Pcenter(1))*(Pgenes(i+atomn)-Pcenter(1))+(Pgenes(i+2*atomn)-Pcenter(2))*(Pgenes(i+2*atomn)-Pcenter(2)))
      Rcp(i) = math.sqrt(math.pow(CPgenes(i+atomn)-CPcenter(0),2)+math.pow(CPgenes(i+2*atomn)-CPcenter(1),2)+math.pow(CPgenes(i+3*atomn)-CPcenter(2),2))
    }
    //排序
    var tempc = 0.0
    var tempp = 0.0
    var tempcp = 0.0
    for(i<-0 until atomn)
    {
      for(j <- i+1 until atomn)
      {
        if(Rc(i)<Rc(j))
        {
          tempc = Rc(i)
          Rc(i) = Rc(j)
          Rc(j) = tempc
        }
        if(Rp(i)<Rp(j))
        {
          tempp = Rp(i)
          Rp(i) = Rp(j)
          Rp(j) = tempp
        }
        if(Rcp(i)<Rcp(j))
        {
          tempcp = Rcp(i)
          Rcp(i) = Rcp(j)
          Rcp(j) = tempcp
        }
      }
    }
    //对比
    for(i<-0 until atomn)
    {
      qc = qc + math.pow(Rcp(i)-Rc(i),2)
      qp = qp + math.pow(Rcp(i)-Rp(i),2)
    }
    qc = math.sqrt(qc/atomn)
    qp = math.sqrt(qp/atomn)
    Sc = 1 / (1 + qc)
    Sp = 1 / (1 + qp) 
    
    /*输出*/
    println("相似度Co: "+Sc+" 相似度Pt: "+Sp)
    val fn = "./result/db38/评价Co"+CoN+"Pt"+PtN+".txt"
    val Print = new PrintWriter(new File(fn))
    Print.println(atomn);
    Print.println("Co相似:\t"+Sc)
    Print.println("Pt相似:\t"+Sp)
    
    /*偏聚*/
    var R_c = 0.0
    var R_p = 0.0
    for(i <- 0 until atomn)
    {
      println(CPgenes(i)+" "+Rcp(i))
      if(CPgenes(i)==0.0)
      {
        
        R_c += Rcp(i)
      }
      else
      {
        
        R_p += Rcp(i)
      }
    }
    R_c = R_c/CoN
    R_p = R_p/PtN
    println("偏聚Co: "+R_c+" 偏聚Pt: "+R_p)
    Print.println("Co偏聚:\t"+R_c)
    Print.println("Pt偏聚:\t"+R_p)
    Print.close()//关闭文件流
  }
  
}