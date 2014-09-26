package sloshy

import akka.actor.ActorRef
import akka.pattern.{ ask, pipe }
import akka.util.Timeout

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent._
import scala.io.Source
import scala.util.Random

object SloshyMain {
  val unitsPerLayer = List(400, 25, 10)
  val trainRatio = 0.8
  val visualize = true
  val iterations = 5
  val printResults = true //false//

  /*
   * Always need these
   */
  val random = new Random
  //  implicit val timeout = Timeout(timeoutAfterFiveSeconds)

  val (trainY, testY, trainX, testX) = {
    def load(f: String): List[List[Double]] = {
      val source = Source.fromFile(f).getLines
      source.map(_.split(",").toList.map(_.toDouble)).toList
    }
    def partBySelection[T](data: List[List[T]], selection: IndexedSeq[Boolean]): (List[List[T]], List[List[T]]) = {
      val (p1, p2) = data.zipWithIndex.partition{ case (line, index) => selection(index) }
      (p1.map(_._1), p2.map(_._1))
    }

    def shuffleIndex(size: Int) = // (0 until size) // in order
      (0 until size).map(i => (i, random.nextDouble)).sortBy(_._2).map(_._1) // random order

    def shuffle[T](data: List[List[T]], indices: IndexedSeq[Int]): List[List[T]] = {
      val m = data.zipWithIndex.map{ case (line, index) => index -> line }.toMap
      (indices map m).toList
    }

    println("Loading Data for X...")
    val fullX = load("ex4data1.csv")

    println("Loading Data for y...")
    val y = load("ex4data1Y.csv")

    val m = fullX.length
    val trainCount = (trainRatio * m).toInt
    val testCount = m - trainCount
    println(s"Using a $trainCount element training set and a $testCount element test set")

    val stableSelection = (0 until m).map(_ => random.nextDouble < SloshyMain.trainRatio)
    val (trainX, testX) = partBySelection(fullX, stableSelection)
    val (trainY, testY) = partBySelection(y, stableSelection)

    val trainShuffle = shuffleIndex(trainX.size)
    val testShuffle = shuffleIndex(testX.size)

    (shuffle(trainY, trainShuffle).map(_.head.toInt),
      shuffle(testY, testShuffle).map(_.head.toInt),
      shuffle(trainX, trainShuffle),
      shuffle(testX, testShuffle))
  }

  def main(args: Array[String]): Unit = {
    println("Sloshy starting!")

    import Sentinel._
    val sentinel = system.actorOf(Sentinel.props(visualize), name = "Sentinel")
    val awaitTimeout = 30.seconds

    def trainWithData(dataX: List[List[Double]], dataY: List[Int]): (Double, Double) = {
      val m = dataX.length
      val zippedResults = for ((x, y) <- dataX zip dataY) yield {
        val responder = (sentinel ? TrainWith(x, y)).mapTo[TrainError]
        val TrainError(yGuess, cumulativeError) = Await.result(responder, awaitTimeout)
        (cumulativeError, if (y == yGuess) 1 else 0)
      }
      val (iterationErrors, rightAnswers) = zippedResults.unzip
      val squaredError = iterationErrors.sum / m
      val percentCorrect = rightAnswers.sum.toDouble * 100 / m
      (percentCorrect, squaredError)
    }

    def evaluateData(dataX: List[List[Double]], dataY: List[Int]): Double = {
      val m = dataX.length
      val rightAnswers = for ((x, y) <- dataX zip dataY) yield {
        val responder = (sentinel ? Request(x)).mapTo[Response]
        val Response(yGuess) = Await.result(responder, awaitTimeout)
        if (y == yGuess) 1 else 0
      }
      val percentCorrect = rightAnswers.sum.toDouble * 100 / m
      percentCorrect
    }

//    val (baseTime, baselinePercentCorrect) = time { evaluateData(testX, testY) }
//    println(f"Baseline (random) answers $baselinePercentCorrect%.2f%% correct ($baseTime%.3f seconds)")

    for (iteration <- 1 to iterations) {
      val (iterTime, (percentCorrect, squaredError)) = time { trainWithData(trainX, trainY) }
      println(f" - Iteration $iteration: average squared error=$squaredError%.5f;" +
        f" answers $percentCorrect%.2f%% correct ($iterTime%.3f seconds)")
    }

    if (printResults) {
      val (trainTime, trainPercentCorrect) = time { evaluateData(trainX, trainY) }
      println(f"Final answers - training set: $trainPercentCorrect%.2f%% correct ($trainTime%.3f seconds)")

      val (testTime, testPercentCorrect) = time { evaluateData(testX, testY) }
      println(f"Final answers - testing set: $testPercentCorrect%.2f%% correct ($testTime%.3f seconds)")
    }

    system.shutdown
    println("Sloshy done.")
  }
  //Loading Data for X...
  //Loading Data for y...
  //Using a 4000 element training set and a 1000 element test set
  //Sloshy starting!
  // - Iteration 1: average squared error=0.35797; answers 74.43% correct (45.489 seconds)
  // - Iteration 2: average squared error=0.16321; answers 90.67% correct (41.768 seconds)
  // - Iteration 3: average squared error=0.12761; answers 92.59% correct (42.061 seconds)
  // - Iteration 4: average squared error=0.10601; answers 93.88% correct (42.087 seconds)
  // - Iteration 5: average squared error=0.09258; answers 94.36% correct (41.417 seconds)
  // - Iteration 6: average squared error=0.08003; answers 95.47% correct (41.882 seconds)
  // - Iteration 7: average squared error=0.07458; answers 95.68% correct (41.328 seconds)
  // - Iteration 8: average squared error=0.06856; answers 95.93% correct (40.856 seconds)
  // - Iteration 9: average squared error=0.06241; answers 96.26% correct (42.304 seconds)
  // - Iteration 10: average squared error=0.05849; answers 96.54% correct (42.563 seconds)
  // - Iteration 11: average squared error=0.05728; answers 96.66% correct (41.725 seconds)
  // - Iteration 12: average squared error=0.05073; answers 96.89% correct (41.798 seconds)
  // - Iteration 13: average squared error=0.04808; answers 97.12% correct (43.063 seconds)
  // - Iteration 14: average squared error=0.04301; answers 97.32% correct (44.180 seconds)
  // - Iteration 15: average squared error=0.04065; answers 97.32% correct (42.977 seconds)
  //Final answers - training set: 97.24% correct (25.652 seconds)
  //Final answers - testing set: 92.64% correct (6.712 seconds)
  //Sloshy done.

  /*
     * Frailty trimming prevents some overfitting and improves testing set
     * performance!
     */

  //With frailty trimming weightDecayRate = 1.0 / 500.0, frailtyLimit = 1.0 / 25.0 
  // - Iteration 1: average squared error=0.35111; answers 75.44% correct (48.088 seconds)
  // - Iteration 2: average squared error=0.16623; answers 89.82% correct (44.602 seconds)
  // - Iteration 3: average squared error=0.12835; answers 92.47% correct (43.443 seconds)
  // - Iteration 4: average squared error=0.10768; answers 93.75% correct (41.601 seconds)
  // - Iteration 5: average squared error=0.09298; answers 94.72% correct (41.547 seconds)
  //Final answers - training set: 94.95% correct (21.851 seconds)
  //Final answers - testing set: 91.12% correct (5.437 seconds)
  //Sloshy done.

  //With frailty trimming weightDecayRate = 1.0 / 500.0, frailtyLimit = 1.0 / 50.0 
  // - Iteration 1: average squared error=0.34471; answers 75.66% correct (44.980 seconds)
  // - Iteration 2: average squared error=0.16377; answers 90.09% correct (44.136 seconds)
  // - Iteration 3: average squared error=0.12468; answers 92.46% correct (44.250 seconds)
  // - Iteration 4: average squared error=0.10362; answers 93.88% correct (43.329 seconds)
  // - Iteration 5: average squared error=0.09163; answers 94.71% correct (43.163 seconds)
  //Final answers - training set: 95.11% correct (22.604 seconds)
  //Final answers - testing set: 93.46% correct (5.519 seconds)
  //Sloshy done.

  //With frailty trimming weightDecayRate = 1.0 / 500.0, frailtyLimit = 1.0 / 100.0 
  // - Iteration 1: average squared error=0.34013; answers 76.52% correct (45.268 seconds)
  // - Iteration 2: average squared error=0.15244; answers 91.03% correct (44.362 seconds)
  // - Iteration 3: average squared error=0.11607; answers 93.32% correct (45.388 seconds)
  // - Iteration 4: average squared error=0.09979; answers 94.17% correct (45.439 seconds)
  // - Iteration 5: average squared error=0.08607; answers 95.18% correct (44.637 seconds)
  //Final answers - training set: 95.83% correct (23.197 seconds)
  //Final answers - testing set: 91.65% correct (5.971 seconds)
  //Sloshy done.
}