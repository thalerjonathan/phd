package wildfire.ui

import scala.swing._
import java.awt.{Graphics2D,Color}

object TestUI {
  def main(args: Array[String]) {
    var xDim = 100;
    var yDim = 100;
    
    val ui = new WildFireUI(xDim, yDim)
    ui.visible = true

    /*
    for (x <- 0 until xDim - 1 ) {
      for (y <- 0 until yDim - 1) {
        ui.fuelChanged(Tuple2(x, y), Math.random());
      }
    }
      */
    
    while ( true ) {
      var randX = Math.random() * xDim.toDouble;
      var randY = Math.random() * yDim.toDouble;
      
      ui.fuelChanged(Tuple2(randX.toInt, randY.toInt), Math.random() );
      
      Thread.sleep( 1 );
    }
  }
}

class WildFireUI(xDim : Int, yDim : Int) extends MainFrame {
  title = "WildFire"
  preferredSize = new Dimension(640, 480)
 
  val fireCells = new FireCells(xDim, yDim)
  val fireCanvas = new WildFireCanvas( fireCells )
  
  contents = new BorderPanel {
    border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
    add(fireCanvas, BorderPanel.Position.Center)
  }
  
  def fuelChanged(coords: Tuple2[Int, Int], fuel: Double) = {
    fireCells.updateFuel(coords, fuel);
    fireCanvas.repaint();
  }
  
  def cellStartBurning(coords: Tuple2[Int, Int]) = {
    fireCells.startBurning(coords);
    fireCanvas.repaint();
  }
  
  def cellStopBurning(coords: Tuple2[Int, Int]) = {
    fireCells.stopBurning(coords);
    fireCanvas.repaint();
  }
}

class WildFireCanvas(val fireCells: FireCells) extends Component {
  override def paintComponent(g : Graphics2D) {
    val xDim = fireCells.getXDim();
    val yDim = fireCells.getYDim();
    
    val d = size
    g.setColor(Color.white);
    g.fillRect(0,0, d.width, d.height);
    val rowWid = d.height / yDim
    val colWid = d.width / xDim
    val wid = rowWid min colWid
    val x0 = (d.width - xDim * wid)/2
    val y0 = (d.height - yDim * wid)/2
    for (x <- 0 until xDim - 1 ) {
      for (y <- 0 until yDim - 1) {
        val fuel = fireCells.getFuel(x, y);
        val isBurning = fireCells.isBurning(x, y);
        var c = Color.RED;
        if ( false == isBurning )
          c = new Color( 0.0f, 1.0f - fuel.toFloat, 0.0f );
        
	      g.setColor(c);
	      g.fillRect(x0 + x * wid, y0 + y * wid, wid, wid)
      }
    }
  }
}

class FireCells(xDim : Int, yDim : Int) {
  private val fuelCells = Array.ofDim[Double](xDim, yDim)
  private val burningCells = Array.ofDim[Boolean](xDim, yDim)
  
  def startBurning(coords: Tuple2[Int, Int]) = {
    burningCells(coords._1)(coords._2) = true;
  }
  
  def stopBurning(coords: Tuple2[Int, Int]) = {
    burningCells(coords._1)(coords._2) = false;
  }
  
  def updateFuel(coords: Tuple2[Int, Int], fuel: Double) = {
    fuelCells(coords._1)(coords._2) = fuel;
  }
  
  def isBurning(x: Int, y: Int) = {
    burningCells(x)(y)
  }
  
  def getFuel(x: Int, y: Int) = {
    fuelCells(x)(y)
  }
  
  def getXDim() = {
    xDim;
  }
  
  def getYDim() = {
    yDim;
  }
}