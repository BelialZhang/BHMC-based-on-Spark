package ga

import scala.collection.mutable.ListBuffer


object TournamentSelection {

  def select(population: Array[(Int, Chromosome)], arity: Int = 10): (Chromosome, Chromosome) = {
    (tournament(population, arity), tournament(population, arity))
  }

  def tournament(population: Array[(Int, Chromosome)], arity: Int): Chromosome = {
    var chromosomes = population.clone()
    val tournaments = new ListBuffer[(Int, Chromosome)]
    for (i <- 0 until arity) yield {
      //val rind = chromosomes.length - 1 - scala.util.Random.nextInt(chromosomes.length / 2)
      val rind = scala.util.Random.nextInt(chromosomes.length)
      tournaments.append(chromosomes(rind))
      chromosomes = chromosomes.slice(0, rind) ++ chromosomes.slice(rind+1, chromosomes.length)
    }
    tournaments.maxBy(i => i._2.fitness)._2
  }

  def pickOne(tournaments: List[(Int, Chromosome)]): Chromosome = {
    tournaments.maxBy(i => i._2.fitness)._2
  }

}

