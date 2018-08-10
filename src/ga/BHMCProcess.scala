package ga
import Array._
import scala.util.Random
import scala.math._
//BHMC �㷨��������
object BHMCProcess {
  
//����Ŷ�������
 def remake(origin: Chromosome,atomN: Int,rate: Double): Chromosome = {
    val length = origin.genes.length
    val N = 4*atomN
    var ch = 0.0
    var temp = 0
    val s = 0.5
    val b = Array.fill(3*atomN)(2*(Random.nextDouble()-0.5))//-1to1num
    var mutateIndex = scala.util.Random.nextInt(atomN)
    var remutateIndex = scala.util.Random.nextInt(atomN)
    //ʵ��ǰ���01remake�仯
    ch = origin.genes(mutateIndex);
		while(origin.genes(remutateIndex) == ch)
				{
					remutateIndex += 1 
					if(remutateIndex == atomN)
					remutateIndex = 0
				}	
				origin.genes(mutateIndex) = origin.genes(remutateIndex)
				origin.genes(remutateIndex) = ch;	
    
		//ʵ�ֺ���Ľṹ����remake�仯
		for(i <- 0 until atomN)//ÿ��λ������Ļ���֮�ϱ仯
		{
		  origin.genes(i+atomN) += b(i)*s
//		  if(origin.genes(i+atomN)>4 | origin.genes(i+atomN) <(-4))
//		    println("remake1",origin.genes(i+atomN))
		  
		  origin.genes(i+2*atomN) += b(i+atomN)*s
//		  if(origin.genes(i+2*atomN)>4 | origin.genes(i+2*atomN) <(-4))
//		    println("remake2",origin.genes(i+2*atomN))
		  
		  origin.genes(i+3*atomN) += b(i+2*atomN)*s
//		  if(origin.genes(i+3*atomN)>4 | origin.genes(i+3*atomN) <(-4))
//		  {
//		    println("remake2",origin.genes(i+3*atomN))
//		  }
		  
		  
		}
		
		var remakenergy = Tool.localEnergy(origin.genes, atomN)
		println("�Ŷ���Ľ���ǣ�" + remakenergy)
    var newGenes = origin.genes.slice(0, N)
    new Chromosome(newGenes)
  }
  
  
  //SphereCut
  def SphereCut[T](first: Chromosome, second: Chromosome,child1: Chromosome,child2: Chromosome,atomN: Int,rate: Double): Double = {
    val numVars = 4*atomN//
    //�Խṹ���ֵ�ȡֵ
    val start = scala.util.Random.nextInt(numVars-atomN) + atomN
    val length = scala.util.Random.nextInt(numVars - start) + 1
    var Energy = 0.0
    var temp = 0
    var dNnm = Array[Double](2)
    //crossover��Ҫ��֤��ԭ�ӱ�������
    var point_left = scala.util.Random.nextInt(atomN)
    var point_right = scala.util.Random.nextInt(atomN)
    if(point_right < point_left)
    {
      temp = point_left
      point_left = point_right
      point_right = temp
            
    }
//    for(i <- point_left to point_right)
//			{
//				(first.genes(i) < 0.5)?(dNum(0)--):(dNum(0));
//				(A[index[i]].cood[j] == 1)?(dNum[1]--):(dNum[1]);
//			
//				(A[index[i+1]].cood[j] == 0)?(dNum[0]++):(dNum[0]);
//				(A[index[i+1]].cood[j] == 1)?(dNum[1]++):(dNum[1]);//a�ж�A[index[i]]��A[index[i+1]]�Ķ�Ӧλ�õ�coor�Ƿ�һ��
//
//		/////////////////////////////////////////////////////////////���һ�£���ôΪ0ʱdNum[0]Ϊ0��Ϊ1ʱdNum[1]Ϊ0�������һ�£���ôΪ0ʱdNum[0]Ϊ1��Ϊ1ʱdNum[1]Ϊ1����ʱ��Ϊ0������һ�����ж�
//						///////////////////////////////////////////////�õ����  �������������01��������ƥ�����Ҫ���д���
//				ch = A[index[i]].cood[j];
//				A[index[i]].cood[j] = A[index[i+1]].cood[j];
//				A[index[i+1]].cood[j] = ch;//////////////////////////����A[index[i]]��A[index[i+1]]��ѡ����ڵ��Ų�������õ���ͬ�Ӵ���
//			}
// 
    //�����Ŷ��ľ����������
//    var child1 = new Array[Double](numVars)
//    var child2 = new Array[Double](numVars)
    var existId = new Array[Int](atomN)
    var frd = new Array[Double](atomN)
    var mrd = new Array[Double](atomN)
    var frdrank = new Array[Double](atomN)
    var mrdrank = new Array[Double](atomN)
    var frcenter = new Array[Double](3)
    var mrcenter = new Array[Double](3)
    var kexist = 0
    var kmid = 0
    var ncross = 0
    var mid =0.0 
    var k = 0
  
    var fr_coor = new Array[Double](atomN)
    var fr_x = new Array[Double](atomN)
    var fr_y = new Array[Double](atomN)
    var fr_z = new Array[Double](atomN)
    var mr_coor = new Array[Double](atomN)
    var mr_x = new Array[Double](atomN)
    var mr_y = new Array[Double](atomN)
    var mr_z = new Array[Double](atomN)
    
    
    for(i<-0 until atomN)
    {
      fr_x(i) = first.genes(i + atomN)
      fr_y(i) = first.genes(i + 2*atomN)
      fr_z(i) = first.genes(i + 3*atomN)
      mr_x(i) = second.genes(i + atomN)
      mr_y(i) = second.genes(i + 2*atomN)
      mr_z(i) = second.genes(i + 3*atomN) 
    }
    //���㸸����ĸ��������
    for(i<-0 until atomN)
    {
      frcenter(0) += fr_x(i)/atomN
      frcenter(1) += fr_y(i)/atomN
      frcenter(2) += fr_z(i)/atomN
      mrcenter(0) += mr_x(i)/atomN
      mrcenter(1) += mr_y(i)/atomN
      mrcenter(2) += mr_z(i)/atomN
    }
    
      for(i<-0 until atomN)
    {
      fr_x(i) -= frcenter(0)
      fr_y(i) -= frcenter(1)
      fr_z(i) -= frcenter(2)
      mr_x(i) -= mrcenter(0)
      mr_x(i) -= mrcenter(1)
      mr_x(i) -= mrcenter(2)
    }
    
     
       for(i<-0 until atomN)
    {
      frd(i) = math.sqrt(fr_x(i)*fr_x(i) + fr_y(i)*fr_y(i) + fr_z(i)*fr_z(i))
      mrd(i) = math.sqrt(mr_x(i)*mr_x(i) + mr_y(i)*mr_y(i) + mr_z(i)*mr_z(i))
      frdrank(i) = frd(i)
      mrdrank(i) = mrd(i)
    }
       
    
    for( k<-0 until atomN-1){
		for(j<-k+1 until atomN){
			if(frd(k)==frd(j)){
				frd(k)=frd(k)+(2 + Random.nextDouble())*(1e-16)
				frdrank(k)=frd(k)
			}
			if(mrd(k)==mrd(j)){
				mrd(k)=mrd(k)+(2 + Random.nextDouble())*(1e-16)
				mrdrank(k)=mrd(k)
			  }
		  }
	  }
    //rank sort
    for(k<-0 until atomN-1){
		for(j<-k+1 until atomN){
			if(frdrank(k)>frdrank(j)){
				mid=frdrank(k)
				frdrank(k)=frdrank(j)
				frdrank(j)=mid;
			}
			if(mrdrank(k)>mrdrank(j)){
				mid=mrdrank(k)
				mrdrank(k)=mrdrank(j)
				mrdrank(j)=mid;
			  }
		  }
	  }
    //���ص����ֵ�ԭ�ӽ�����������Ҫ��֤ԭ�ӵı����͸���
	  for(k<-1 until atomN-1){
		  if(!((frdrank(k-1)>mrdrank(k)) || (frdrank(k)<mrdrank(k-1)))){
			  existId(kexist) = k
		  	kexist=kexist+1
		  }
	  }
    //////////////////////////
    if(kexist!=0){
	  	kmid = math.ceil(kexist*Random.nextDouble()).toInt
		if(kmid==0)
			kmid = 1
			
		ncross=existId(kmid-1)
		k=0
		for(j<-0 until atomN){
			if(frd(j)<frdrank(ncross)-1e-16){
			 // child1(k) = fr_coor(j);
				child1.genes(k + atomN) = fr_x(j);
				child1.genes(k + 2*atomN) = fr_y(j);
				child1.genes(k + 3*atomN) = fr_z(j);
				k=k+1;
			}
			if(mrd(j)>mrdrank(ncross-1)+1e-16){
			  //child1(k) = mr_coor(j);
				child1.genes(k + atomN) = mr_x(j);
				child1.genes(k + 2*atomN) = mr_y(j);
				child1.genes(k + 3*atomN) = mr_z(j);
				k=k+1;
			}
		}
	  	
		k=0
		for(j<-0 until atomN){
			if(frd(j)>frdrank(ncross-1)+1e-16){
			  //child2(k) = fr_coor(j);
				child2.genes(k + atomN) = fr_x(j);
				child2.genes(k + 2*atomN) = fr_y(j);
				child2.genes(k + 3*atomN) = fr_z(j);
				k=k+1;
			}
			if(mrd(j)<mrdrank(ncross)-1e-16){
				//child2(k) = mr_coor(j);
				child2.genes(k + atomN) = mr_x(j);
				child2.genes(k + 2*atomN) = mr_y(j);
				child2.genes(k + 3*atomN) = mr_z(j);
				k=k+1;
			}
		}
		
	}
	else{
		for(j<-0 until 4 * atomN){
			child1.genes(j) = first.genes(j)
			child2.genes(j) = second.genes(j)
		}
	}
   
    for(j<-0 until atomN){
			child1.genes(j) = first.genes(j)
			child2.genes(j) = second.genes(j)
		}
	  var child1energy = Tool.localEnergy(child1.genes, atomN)
    var child2energy = Tool.localEnergy(child2.genes, atomN)
    Energy = child1energy
		println("111SphereCut��Ľ���ǣ�" + child1energy)
		println("222SphereCut��Ľ���ǣ�" + child2energy)
		if(child1energy > child2energy)
		  {
		    child1.genes = child2.genes
		    Energy = child2energy
		  }
    Energy
 
  }
   
   //�ṹ�������
  def exchange(origin: Chromosome,atomN: Int,rate: Double): Double = {
    val length = origin.genes.length
    var Energy  = 0.0
    val N = 4*atomN
    var ch = 0.0
    var temp = 0
    val s = 0.35
    val r0 = 2
    var dis = new Array[Double](atomN)
    //c++δ����maxtemp��Ĭ��Ϊ0
    var maxtemp = Tool.distool(origin.genes,dis,atomN)
    for(i <- 0 until atomN){
     if(dis(i) >= temp-0.1) 
     //if(dis(i) >= maxtemp-0.1)
     {
       origin.genes(i + atomN) = (Random.nextDouble()-0.5)*r0*math.pow(atomN,1.0/3.0)
       origin.genes(i + 2*atomN) = (Random.nextDouble()-0.5)*r0*math.pow(atomN,1.0/3.0)
       origin.genes(i + 3*atomN) = (Random.nextDouble()-0.5)*r0*math.pow(atomN,1.0/3.0)
//       if(origin.genes(i+atomN)>4 | origin.genes(i+atomN) <(-4))
//         println("exchange1",origin.genes(i + atomN))
//       if(origin.genes(i+2*atomN)>4 | origin.genes(i+2*atomN) <(-4))
//         println("exchange2",origin.genes(i + 2*atomN))
//       if(origin.genes(i+3*atomN)>4 | origin.genes(i+3*atomN) <(-4))
//         println("exchange3",origin.genes(i + 3*atomN))
     }
    }
   Energy = Tool.localEnergy(origin.genes, atomN)
//    println("�仯��������ǣ�" + originenergy)
   /*�ɰ�������*/
    var newGenes = origin.genes.slice(0, N)
    new Chromosome(newGenes)
   
    Energy
  }
  /*�˴��ɰ�û��*/
  def ceil (x:Double) :Double = java.lang.Math.ceil(x);
  
}

