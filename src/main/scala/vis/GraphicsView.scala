package vis

import java.awt.{ Color, Frame, Window, GraphicsEnvironment }
import java.awt.event.{ MouseAdapter, MouseEvent }

case class Point(x: Int, y: Int)
case class Rectangle(min: Point, max: Point) {
  def contains(p: Point) =
    (p.x >= min.x) && (p.x <= max.x) && (p.y >= min.y) && (p.y <= max.y)
}

/*
 * The grid is a rectangle of logical coordinates, mapped to actual screen coordinates
 */
class GraphicsView(grid: Rectangle, drawGrid: Boolean) {
  def this(xRange: Int, yRange: Int, drawGrid: Boolean = true) =
    this(Rectangle(Point(-xRange / 2, -yRange / 2), Point(xRange / 2, yRange / 2)), drawGrid)

  val numBuffers = 2 // Includes front buffer

  val (backgroundColor, neuronOutline) = (Color.white, Color.blue)
  val (axisColor, gridColor) = (Color.red, Color.pink)

  val screenDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice

  /*
   *  Create a window for full-screen mode
   *  Configure the window so that a mouse click will exit full-screen mode
   */
  val window = new Window(new Frame(screenDevice.getDefaultConfiguration))
  val windowCloser = new MouseAdapter {
    override def mousePressed(evt: MouseEvent) = {
      println("Mouse pressed")
      cleanUp
    }
  }
  window addMouseListener windowCloser

  /*
   * Enter full-screen mode and create the back buffer
   * Get graphics context for drawing to the window
   */
  screenDevice setFullScreenWindow window
  window.requestFocus
  window createBufferStrategy numBuffers
  val strategy = window.getBufferStrategy
  private var graphics = strategy.getDrawGraphics // var so we can flip it

  // val capabilities = screenDevice.getDefaultConfiguration.getBufferCapabilities
  // if (!capabilities.isPageFlipping) sys.error("Page flipping is NOT supported")

  /*
   * Get actual screen size, keep cells square, establish padding
   */
  val screen = Rectangle(Point(0, 0), Point(window.getWidth, window.getHeight))
  val rangeGrid = Point (grid.max.x - grid.min.x + 1, grid.max.y - grid.min.y + 1)
  val gridScreenRatio = math.max(1, math.min(screen.max.x / rangeGrid.x, screen.max.y / rangeGrid.y))
  val padScreen = Point(
    (screen.max.x - (gridScreenRatio * rangeGrid.x)) / 2,
    (screen.max.y - (gridScreenRatio * rangeGrid.y)) / 2
  )
  //  println("View created:\n" +
  //    s" - grid range=$grid\n" +
  //    s" - screen size=$screen\n" +
  //    s" - cell screen size=$gridScreenRatio\n" +
  //    s" - pad=$padScreen")

  /*
   * Do initial background display
   */
  setBackground
  //display

  private def screenXForGridX(x: Int) = (x - grid.min.x) * gridScreenRatio + padScreen.x
  private def screenYForGridY(y: Int) = screen.max.y - ((y - grid.min.y) * gridScreenRatio + padScreen.y)
  private def screenForGrid(p: Point) = Point(screenXForGridX(p.x), screenYForGridY(p.y))

  private def cleanUp: Unit = {
    screenDevice setFullScreenWindow null
    System exit 0
  }

  /*
   * Clear background and draw coordinates
   */
  def setBackground: Unit = {
    graphics.setColor(backgroundColor)
    graphics.fillRect(screen.min.x, screen.min.y, screen.max.x, screen.max.y)

    if (drawGrid) {
      val fromScreenY = screenYForGridY(grid.min.y)
      val toScreenY = screenYForGridY(grid.max.y + 1)
      for (gridX <- grid.min.x to grid.max.x + 1) {
        graphics.setColor(if (gridX == 0) axisColor else gridColor)
        val screenX = screenXForGridX(gridX)
        graphics.drawLine(screenX, fromScreenY, screenX, toScreenY)
      }

      val fromScreenX = screenXForGridX(grid.min.x)
      val toScreenX = screenXForGridX(grid.max.x + 1)
      for (gridY <- grid.min.y to grid.max.y + 1) {
        graphics.setColor(if (gridY == 0) axisColor else gridColor)
        val screenY = screenYForGridY(gridY)
        graphics.drawLine(fromScreenX, screenY, toScreenX, screenY)
      }
    }
  }

  /*
   * Draw on the back buffer (not displayed until flipped)
   */
  def drawPoint(p: Point, color: Color): Unit = if (grid contains p) {
    graphics setColor color
    val sp = screenForGrid(p)
    graphics.fillOval(sp.x - 2, sp.y - 2, 4, 4)
  } else {
    println(s"grid $grid does not contain point $p")
  }

  def drawCircle(center: Point, radius: Int,
                 color: Color = neuronOutline, outlineColor: Option[Color] = None): Unit = {
    graphics.setColor(color)
    val sp = screenForGrid(center)
    val sRadius = radius * gridScreenRatio
    graphics.fillOval(sp.x - sRadius, sp.y - sRadius, sRadius * 2, sRadius * 2)
    outlineColor.map{ color =>
      graphics.setColor(color)
      graphics.drawOval(sp.x - sRadius, sp.y - sRadius, sRadius * 2, sRadius * 2)
    }
  }

  def drawLine(from: Point, to: Point, color: Color = neuronOutline): Unit = {
    graphics.setColor(color)
    val sFrom = screenForGrid(from)
    val sTo = screenForGrid(to)
    graphics.drawLine(sFrom.x, sFrom.y, sTo.x, sTo.y)
  }

  def drawPoint(p: Point, isAlive: Boolean = true): Unit =
    drawPoint(p, if (isAlive) neuronOutline else backgroundColor)

  /*
   * Done drawing - dispose graphics on the back buffer
   * Flip the back buffer to the screen
   * Get the graphics for the new back buffer
   */
  def display {
    graphics.dispose
    strategy.show
    graphics = strategy.getDrawGraphics
  }
}