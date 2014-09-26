package model

import sloshy.Dendrite
import java.awt.Color
import vis.{ GraphicsView, Point }
import akka.actor.{ Actor, ActorSystem, ActorRef, ActorLogging, Props }
//import akka.dispatch.{ RequiresMessageQueue, BoundedMessageQueueSemantics }

object ModelView {
  /*
   * Message definitions
   */
  case class NeuronUpdate(layer: Int, index: Int,
                          value: Double, biasWeight: Double, dendrites: Iterable[Dendrite])

  case class RemoveDendrite(fromLayer: Int, fromIndex: Int,
                            toLayer: Int, toIndex: Int)
  /*
   * Properties for initialization
   */
  def props(unitsPerLayer: List[Int]): Props = Props(new ModelView(unitsPerLayer))
}

class ModelView(val unitsPerLayer: List[Int]) extends Actor
    //with RequiresMessageQueue[BoundedMessageQueueSemantics]
    with ActorLogging {

  import ModelView._

  override def preStart = log debug "Starting"
  override def preRestart(reason: Throwable, message: Option[Any]) {
    log error (reason, "Restarting due to [{}] when processing [{}]",
      reason.getMessage, message getOrElse "")
  }

  /*
   * Fixed parameters - minimums for most crowded layer
   */
  val (circleRadiiMin, circleRadiiMax) = (1, 12)
  val minCircleSep = 1

  val horz = (unitsPerLayer.max) * (circleRadiiMin * 2 + minCircleSep) - minCircleSep
  val layerSep = (horz * 8 / 16) / (unitsPerLayer.size - 1) // approx optimum
  val vert = (unitsPerLayer.size - 1) * layerSep + circleRadiiMin * 2

  val circleRadii =
    for ((units, i) <- unitsPerLayer.zipWithIndex) yield if (i == 0) circleRadiiMin else {
      val calcRadius = ((horz + minCircleSep) / units - minCircleSep) / 2
      math.min(12, calcRadius)
    }

  def centerOf(index: Int, layer: Int) = {
    val x = horz / (unitsPerLayer(layer) - 1) * index - (horz / 2)
    val y = (vert / 2) - (layer * layerSep) - circleRadii(layer)
    Point(x, y)
  }

  val graphicsView = new GraphicsView(horz, vert, drawGrid = false)
  graphicsView.setBackground

  var maxBiasWeight = 0.0
  var maxDendriteWeight = unitsPerLayer.zipWithIndex.map{ case (_, i) => i -> 0.0 }.toMap

  val totalNeurons = unitsPerLayer.sum
  val displayEvery = 1
  val drawNeuronEvery = 1
  val drawDendriteEvery = 100

  var neuronCount = 0
  var screenCount = 0

  def drawDendrite(toCenter: Point, toLayer: Int)(dendrite: Dendrite) = {
    maxDendriteWeight = maxDendriteWeight.map {
      case (layer, value) if layer == toLayer =>
        layer -> math.max(value, math.abs(dendrite.weight))
      case (layer, value) => layer -> value
    }
    val fromCenter = centerOf(dendrite.upstreamIndex, dendrite.upstreamLayer)
    val revIntensity = 255 - (math.abs(dendrite.weight) / maxDendriteWeight(toLayer) * 255).toInt
    if (revIntensity < 230) { // use to screen out overly-faint connections
      val color = new Color(
        /* red */ if (dendrite.weight < 0) 255 else revIntensity,
        /* green */ revIntensity,
        /* blue */ if (dendrite.weight > 0) 255 else revIntensity, 
        /* alpha */  (255 - revIntensity) / 2 // alpha makes it pretty *and* pretty slow
      )
      graphicsView.drawLine(fromCenter, toCenter, color)
    }
  }

  /*
   * Receive updates
   */
  def receive = {
    case NeuronUpdate(layer, index, value, biasWeight, dendrites) /*if layer > 0*/ =>
      maxBiasWeight = math.max(maxBiasWeight, math.abs(biasWeight))
      val intensity = (math.max(value, 0.0) * 255).toInt
      val actColor = new Color(intensity, intensity, intensity)
      val revIntensity = 255 - (math.abs(biasWeight) / maxBiasWeight * 255).toInt
      val biasColor = if (biasWeight == 0) None else Some(new Color(
        /* red */ if (biasWeight < 0) 255 else revIntensity,
        /* green */ revIntensity,
        /* blue */ if (biasWeight > 0) 255 else revIntensity))
      val center = centerOf(index, layer)

      //println(s"screenCount=$screenCount, neuronCount=$neuronCount")
      if (screenCount % drawDendriteEvery == 0)
        dendrites foreach drawDendrite(center, layer)

      if (screenCount % drawNeuronEvery == 0)
        graphicsView.drawCircle(center, circleRadii(layer), actColor, biasColor)

      neuronCount = if (neuronCount == totalNeurons) 0 else neuronCount + 1
      screenCount =
        if (neuronCount == 0) {
          if (screenCount % displayEvery == 0) {
            graphicsView.display
            if ((screenCount + 1) % drawDendriteEvery == 0)
              graphicsView.setBackground
          }
          screenCount + 1
        } else screenCount

    //    case NeuronUpdate(layer, index, value, biasWeight, dendrites) if layer == 0 =>
    //ignore these
  }

  //  def erase(toCenter: Point, fromCenter: Point) = {
  //    graphicsView.drawLine(fromCenter, toCenter, graphicsView.backgroundColor)
  //  }
}