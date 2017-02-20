package hac.ui

/**
  * Created by jonathan on 06/12/16.
  */
package wildfire.ui

import scala.swing._
import java.awt.{Color, Graphics2D}

import hac.actors.HACAgent

class AgentInfo(val pos: (Double, Double), val hero : Boolean)

class HACFrontend(agentCount : Int) extends MainFrame {
  title = "Heroes & Cowards"
  preferredSize = new Dimension(1000, 1000)

  val agents = new Array[AgentInfo](agentCount);
  val agentsCanvas = new AgentsCanvas( agents )

  contents = new BorderPanel {
    border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
    add(agentsCanvas, BorderPanel.Position.Center)
  }

  def agentUpdate(id : Int, pos: (Double, Double), hero : Boolean) = {
    agents( id ) = new AgentInfo(pos, hero);
    agentsCanvas.repaint();
  }
}

class AgentsCanvas(val agents: Array[AgentInfo]) extends Component {
  val BORDER_X = 10;
  val BORDER_Y = 10;

  val AGENTSIZE = 3;

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

        x = BORDER_X + a.pos._1 * width
        y = BORDER_Y + a.pos._2 * height

        if (a.hero)
          g.setColor(Color.GREEN)
        else
          g.setColor(Color.RED)

        g.fillRect(x.toInt, y.toInt, AGENTSIZE, AGENTSIZE)
      }
    }
  }
}