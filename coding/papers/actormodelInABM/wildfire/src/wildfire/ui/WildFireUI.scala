package wildfire.ui

import wildfire.actors.CellState
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
      
      ui.cellBecomesAlive(Tuple2(randX.toInt, randY.toInt), Math.random() );
      
      Thread.sleep( 1 );
    }
  }
}

class WildFireUI(xDim : Int, yDim : Int) extends MainFrame {
  title = "WildFire"
  preferredSize = new Dimension(640, 480)
 
  val cells = new Cells(xDim, yDim)
  val cellsCanvas = new CellsCanvas( cells )
  
  contents = new BorderPanel {
    border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
    add(cellsCanvas, BorderPanel.Position.Center)
  }
  
  def cellChangedState(coords: Tuple2[Int, Int], state: CellState.Value) = {
    cells.cellChangedState(coords, state);
    cellsCanvas.repaint();
  }
  
  def cellBecomesAlive(coords: Tuple2[Int, Int], fuel: Double) = {
    cells.cellBecomesAlive(coords, fuel);
    cellsCanvas.repaint();
  }
}

class CellsCanvas(val cells: Cells) extends Component {
  override def paintComponent(g : Graphics2D) {
    val xDim = cells.getXDim();
    val yDim = cells.getYDim();
    
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
        val fuel = cells.getFuel(x, y);
        val cellState = cells.getCellState(x, y);
        var c = Color.RED;
        if ( CellState.LIVING == cellState )
          c = new Color( 0.0f, 1.0f - fuel.toFloat, 0.0f );
        else if ( CellState.BURNING == cellState )
          c = Color.RED;
        else if ( CellState.DEAD == cellState )
          c = Color.WHITE;
        
	      g.setColor(c);
	      g.fillRect(x0 + x * wid, y0 + y * wid, wid, wid)
      }
    }
  }
}

class Cells(xDim : Int, yDim : Int) {
  private val cellFuel = Array.ofDim[Double](xDim, yDim)
  private val cellState = Array.ofDim[CellState.Value](xDim, yDim)
  
  def cellChangedState(coords: Tuple2[Int, Int], state: CellState.Value) = {
    cellState(coords._1)(coords._2) = state;
  }
  
  def cellBecomesAlive(coords: Tuple2[Int, Int], fuel: Double) = {
    cellFuel(coords._1)(coords._2) = fuel;
    cellState(coords._1)(coords._2) = CellState.LIVING;
  }
  
  def getCellState(x: Int, y: Int) = {
    cellState(x)(y)
  }
  
  def getFuel(x: Int, y: Int) = {
    cellFuel(x)(y)
  }
  
  def getXDim() = {
    xDim;
  }
  
  def getYDim() = {
    yDim;
  }
}