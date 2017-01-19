package hac.ui

/**
  * Created by jonathan on 06/12/16.
  */
package wildfire.ui

import scala.swing._
import java.awt.{Color, Graphics2D}

import hac.actors.HACAgent

class HACFrontend(agentCount : Int) extends MainFrame {
  title = "Heroes & Cowards"
  preferredSize = new Dimension(700, 700)

  val agents = new Array[HACAgent](agentCount);
  val agentsCanvas = new AgentsCanvas( agents )

  contents = new BorderPanel {
    border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
    add(agentsCanvas, BorderPanel.Position.Center)
  }

  def agentUpdate(a: HACAgent) = {
    val idx = a.getId();

    agents( idx ) = a;
    agentsCanvas.repaint();
  }
}

class AgentsCanvas(val agents: Array[HACAgent]) extends Component {
  val BORDER_X = 10;
  val BORDER_Y = 10;

  val AGENTSIZE = 2;

  override def paintComponent(g : Graphics2D) {
    val d = size
    val width: Double = d.width - 2 * BORDER_X
    val height: Double = d.height - 2 * BORDER_Y

    g.setColor(Color.WHITE)
    g.fillRect(BORDER_X, BORDER_Y, width.toInt, height.toInt)

    var x: Double = .0
    var y: Double = .0

    for (a <- agents) {
      if (a != null) {
        x = BORDER_X + a.getPosition()._1 * width
        y = BORDER_Y + a.getPosition()._2 * height

        if (a.isHero)
          g.setColor(Color.GREEN)
        else
          g.setColor(Color.RED)

        g.fillRect(x.toInt, y.toInt, AGENTSIZE, AGENTSIZE)
      }
    }
  }
}