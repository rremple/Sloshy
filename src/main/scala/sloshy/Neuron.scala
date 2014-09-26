package sloshy

import model.ModelView
import akka.actor.{ Actor, ActorSystem, ActorRef, ActorLogging, Props }
import akka.dispatch.{ RequiresMessageQueue, BoundedMessageQueueSemantics }
import scala.util.Random
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ListBuffer

case class Dendrite(
  upstreamLayer: Int,
  upstreamIndex: Int,
  weight: Double, // current weight
  smoothedWeightMagnitude: Double // weight that changes slowly over time
  )

object Neuron {
  /*
   * Message definitions
   */
  case class ConnectToDownstream(neuron: ActorRef)
  case class ConnectToUpstream(neuron: ActorRef, layer: Int, index: Int)
  case object GetReady

  case class Fire(value: Double)
  case class PropagateSignal(value: Double)
  case class Result(index: Int, value: Double)
  case class Error(value: Double)
  case class PropagateError(value: Double, dendriteDied: Boolean = false)
  case object NoPropagate

  /*
   * Properties for initialization
   */
  def props(layer: Int, index: Int, modelView: Option[(ActorRef, Int)]): Props = Props(new Neuron(layer, index, modelView))
}

class Neuron(val layer: Int, val index: Int, val modelView: Option[(ActorRef, Int)]) extends Actor
    //with RequiresMessageQueue[BoundedMessageQueueSemantics]
    with ActorLogging {

  import Neuron._
  import Sentinel._
  import ModelView._

  override def preStart = log debug "Starting"
  override def preRestart(reason: Throwable, message: Option[Any]) {
    log error (reason, "Restarting due to [{}] when processing [{}]",
      reason.getMessage, message getOrElse "")
  }

  /*
   * Fixed parameters
   */
  val frailtyLimit = 1.0 / 60.0 // ratio of average weight to most frail weight -- frailest are removed
  val weightSmoothingRate = 1.0 / 500.0 // portion given to current vs. past weight values in running average
  val epsilon_init = 0.12
  val alpha = 1.0 // alpha is the learning rate
  val lambda = 0.0 // lambda is the weight given to the regularization term  -- doesn't seem to help:
  //lambda=0.0, 5 iterations - training set: 95.19% correct, testing set: 92.35% correct 
  //lambda=0.1, 5 iterations - training set: 95.03% correct, testing set: 91.02% correct
  //lambda=1.0, 5 iterations - training set: 94.79% correct, testing set: 89.95% correct

  val random = new Random
  def randomWeight = random.nextDouble * (2 * epsilon_init) - epsilon_init

  /*
   * Upstream and downstream connections, used in forward and backpropagation 
   * The bias weight represents a pseudo-input to all neurons,
   * with a fixed value of 1.0.
   */
  var dendrites: Map[ActorRef, Dendrite] = Map() //upstream neurons, with weights
  var downstreamNeurons: List[ActorRef] = Nil //downstream neurons
  var biasWeight = randomWeight
  var newValue = 0.0

  /*
   * Some operations are only valid on input and output nerons. 
   * Consider making these input variables instead of "layer". Right now they
   * are set when we "get ready" for input.
   */
  var isInput = false
  var isOutput = false
  var undisplayedCount = modelView.map(_._2) getOrElse 0

  /*
   * Transient data structures for tracking inputs. Forward propegation only
   * applies logic when all signals are received from upstream neurons, and
   * backpropagation only applies logic when all errors are received from
   * downstream neurons.
   */
  var dendriteSignalsReceived: Map[ActorRef, Double] = Map()
  var dendriteErrorsReceived: List[Double] = Nil

  def allSignalsReceived = (dendrites.size == dendriteSignalsReceived.size)
  def allErrorsReceived = (downstreamNeurons.size == dendriteErrorsReceived.size)

  def display: Unit = modelView foreach {
    case (view, displayEvery) =>
      undisplayedCount =
        if (undisplayedCount + 1 >= displayEvery) {
          view ! NeuronUpdate(layer, index, newValue, biasWeight, dendrites.values)
          0
        } else undisplayedCount + 1

  }

  def resetDendriteContext: Unit = {
    display
    dendriteSignalsReceived = Map()
    dendriteErrorsReceived = Nil
  }

  /*
   * When not in forward propagate, handy to stash messages and then restore
   * them on context switch when we can handle them
   */
  var stash: List[Any] = Nil

  def becomeForwardpropagate: Unit = {
    resetDendriteContext
    stash foreach { self ! _ }
    stash = Nil
    context become forwardpropagate
  }

  /*
   * Logistic function and other learning function methods (see 
   * http://en.wikipedia.org/wiki/Backpropagation#Derivation for more on the math)
   */
  def sigmoid(s: Double): Double = 1.0 / (1.0 + math.exp(-s))
  def sigmoidGradient(s: Double) = sigmoid(s) * (1 - sigmoid(s))

  /*
   * The "net" is the weighted sum of the outputs (signals) from all upstream neurons.
   * This is used both in forward and backpropagation.
   */
  def netSignals: Double =
    dendrites.map{ case (up, dendrite) => dendrite.weight * dendriteSignalsReceived(up) }.sum + biasWeight

  /*
   * Does the backpropegation calculation heavy lifting, including updating 
   * dendrites with new weights (may also drop frail dendrites along the way).
   * The error parameter for an output neuron is just calculted minus expected.
   * For any other neuron it is the sum of all errors received from downstream
   * neurons
   */
  def adjustWeightsAndBackpropagate(error: Double): Unit = {
    /*
     * Regularization: I don't think this part works at all... so use zero or fix it!
     */
    val regularizationTerm = // I don't think this part works at all... so use zero or fix it!
      if (lambda == 0) lambda else lambda * dendrites.values.map(_.weight).sum / dendrites.size

    /*
     * Frailty: the idea is to disconnect exceedingly frail dendrites (as compared to average). 
     * It seems to reduce overfitting (by eliminating unnecessary features) and
     * makes things a bit faster (if many messages are eliminated). The average decayed weight
     * magnitude (always positive) smoothes out the weight over time so we don't
     * shutdown a dendrite with a short run of low magnitude weights.
     */
    val avgSmoothedWeightMagnitude = dendrites.values.map(_.smoothedWeightMagnitude).sum / dendrites.size
    val minSmoothedWeightMagnitude = avgSmoothedWeightMagnitude * frailtyLimit

    /*
     * Multipling delta by the signal received on a dendrite will give you the
     * partial derivitive of the squared error with respect to the weight --
     * exactly what we will need adjust the weight to minimize the error
     */
    val delta = error * sigmoidGradient(netSignals)

    /*
     * Updates dendrites with new weights, eliminates frail dendrites, and
     * propagates errors to upstream neurons.
     */
    dendrites = dendrites flatMap {
      case (up, dendrite) =>
        val deltaError = dendriteSignalsReceived(up) * delta
        val updatedWeight = dendrite.weight - alpha * (deltaError + regularizationTerm)
        
        val updatedSmoothedWeightMagnitude =
          dendrite.smoothedWeightMagnitude * (1.0 - weightSmoothingRate) +
            math.abs(updatedWeight) * weightSmoothingRate
        if (updatedSmoothedWeightMagnitude >= minSmoothedWeightMagnitude) { //not frail
          up ! PropagateError(delta * dendrite.weight)
          Some(up -> dendrite.copy(weight = updatedWeight, smoothedWeightMagnitude = updatedSmoothedWeightMagnitude))
        } else { //frail
          up ! PropagateError(delta * dendrite.weight, dendriteDied = true)
          None
        }
    }
    biasWeight -= alpha * delta
  }

  /*
   *  Initial connections
   */
  def receive = {

    case ConnectToDownstream(neuron) =>
      if (!(downstreamNeurons contains neuron)) {
        downstreamNeurons = neuron :: downstreamNeurons
        neuron ! ConnectToUpstream(self, layer, index)
      }

    case ConnectToUpstream(neuron, upstreamLayer, upstreamIndex) =>
      dendrites += (neuron -> Dendrite(upstreamLayer, upstreamIndex, randomWeight, epsilon_init))

    case GetReady =>
      isInput = dendrites.isEmpty
      isOutput = downstreamNeurons.isEmpty
      if (isInput) biasWeight = 0.0
      becomeForwardpropagate

    case other =>
      stash = other :: stash
  }

  /*
   *  Forward propagation 
   */
  def forwardpropagate: Receive = {

    case Fire(value) if isInput => // will get these while training and not training
      downstreamNeurons.foreach(_ ! PropagateSignal(value))
      context become backpropagate

    case PropagateSignal(value) => // will get these while training and not training
      dendriteSignalsReceived += sender -> value
      if (allSignalsReceived) {
        newValue = sigmoid(netSignals)
        if (isOutput)
          context.parent ! Result(index, newValue)
        else {
          downstreamNeurons foreach (_ ! PropagateSignal(newValue))
          context become backpropagate
        }
      }

    case Error(error) if isOutput => // will get these while training
      adjustWeightsAndBackpropagate(error)
      resetDendriteContext

    case NoPropagate if isOutput => // will get these while not training
      dendrites foreach { case (up, _) => up ! NoPropagate }
      resetDendriteContext

    case other => // uh oh...
      log error (s"received $other from $sender during forward propagation")
  }

  /*
   *  Backpropagation 
   */
  def backpropagate: Receive = {

    case PropagateError(error, dendriteDied) => // will get these while training
      if (dendriteDied) downstreamNeurons = downstreamNeurons filterNot (_ == sender)
      else dendriteErrorsReceived = error :: dendriteErrorsReceived
      if (allErrorsReceived) {
        if (!isInput) adjustWeightsAndBackpropagate(dendriteErrorsReceived.sum)
        becomeForwardpropagate
      }

    case NoPropagate => // will get these while not training
      dendriteErrorsReceived = 0.0 :: dendriteErrorsReceived
      if (allErrorsReceived) {
        dendrites foreach { case (up, _) => up ! NoPropagate }
        becomeForwardpropagate
      }

    case other =>
      stash = other :: stash
  }
}