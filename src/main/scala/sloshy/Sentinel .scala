package sloshy

import akka.actor.{ Actor, ActorSystem, ActorRef, ActorLogging, Props }
import model.ModelView
//import akka.dispatch.{ RequiresMessageQueue, BoundedMessageQueueSemantics }

object Sentinel {
  /*
   * Message definitions
   */
  case class TrainWith(x: List[Double], y: Int)
  case class TrainError(y: Int, cumulativeError: Double)
  case class Request(x: List[Double])
  case class Response(y: Int)

  /*
   * Properties for initialization
   */
  def props(visualize: Boolean): Props = Props(new Sentinel(visualize))
}

class Sentinel(val visualize: Boolean) extends Actor
    //with RequiresMessageQueue[BoundedMessageQueueSemantics]
    with ActorLogging {

  import Sentinel._
  import Neuron._

  override def preStart = log debug "Starting"
  override def preRestart(reason: Throwable, message: Option[Any]) {
    log error (reason, "Restarting due to [{}] when processing [{}]",
      reason.getMessage, message getOrElse "")
  }

  /*
   * Create model/view for visualizing
   */
  val modelView: Option[(ActorRef, Int)] =
    if (!visualize) None
    else Some((context.actorOf(ModelView.props(SloshyMain.unitsPerLayer), name = "ModelView"), 43))

  /*
   * Build up neurons in layers with dendrites between all neurons in each layer
   */
  val (inputNeurons, outputNeurons, allNeurons) = {
    val emptyLayers = List[List[ActorRef]]()
    val constructViaFold = (SloshyMain.unitsPerLayer.zipWithIndex foldLeft emptyLayers) {
      case (priorLayers, (layerSize, layer)) =>
        //val layer = priorLayers.length
        val newLayer = for (index <- 0 until layerSize) yield {
          context.actorOf(Neuron.props(layer, index, modelView), name = s"Neuron-$layer-$index")
        }
        for (
          priorLayer <- priorLayers.headOption;
          upstreamNeuron <- priorLayer;
          newNeuron <- newLayer
        ) { upstreamNeuron ! Neuron.ConnectToDownstream(newNeuron) }
        newLayer.toList :: priorLayers
    }
    val output = constructViaFold.head
    val input = constructViaFold.last
    val all = constructViaFold.reverse.flatten
    (input, output, all)
  }

  Thread sleep 300 // while connections are made -- I wish i didn't have to do this...
  allNeurons foreach (_ ! GetReady)

  /*
   * Transient data structures for tracking expected output, client, errors, and
   * results across multiple messages (e.g., one from each output neuron).
   */
  var trainingY: Option[Int] = None
  var client: Option[ActorRef] = None
  var cumulativeError: Double = 0.0
  var resultsReceived: Map[Int, Double] = Map()

  def allResultsReceived = resultsReceived.size == outputNeurons.size
  def training = trainingY.isDefined

  def resetContext = {
    trainingY = None
    client = None
    cumulativeError = 0.0
    resultsReceived = Map()
  }

  /*
   * Gets the ball rolling -- given a set of inputs and (maybe) an expected
   * output, set the transient info and fire the inputs to the input neurons.
   */
  def fireInputs(x: List[Double], y: Option[Int]) = {
    trainingY = y
    client = Some(context.sender)
    (x zip inputNeurons) foreach { case (value, actor) => actor ! Fire(value) }
  }

  /*
   * Receive client requests as well as results from output neurons.
   */
  def receive = {
    case TrainWith(x, y) => // from client - training
      fireInputs(x, Some(y))

    case Request(x) => // from client - not training (no expected result)
      fireInputs(x, None)

    case Result(index, value) => // from output neuron
      val digit = index + 1
      resultsReceived += digit -> value
      trainingY foreach { y =>
        val target = if (y == digit) 1.0 else 0.0
        val error = value - target
        cumulativeError += math.pow(error, 2)
        context.sender ! Error(error) // if training, error feedback to output neuron
      }
      if (!training) context.sender ! NoPropagate // if not training, no feedback needed
      if (allResultsReceived) client foreach { c =>
        val (mostProbable, _) = resultsReceived.maxBy(_._2) // digit for highest value
        if (training) c ! TrainError(mostProbable, cumulativeError)
        else c ! Response(mostProbable)
        resetContext
      }
  }
}