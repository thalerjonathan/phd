package sg.ui

/**
  * Created by jonathan on 06/12/16.
  */
import java.awt.geom.Rectangle2D
import java.awt.{Color, Graphics2D}

import sg.actors.SGAgent

import scala.swing._

class AgentInfo(val currRole : SGAgent.SGRole.Value, val prevRole : SGAgent.SGRole.Value, val pos: (Int, Int))

class SGFrontend(cols : Int, rows : Int) extends MainFrame {
  title = "Spatial Game"
  preferredSize = new Dimension(1000, 1000)

  val agents = new Array[AgentInfo](cols * rows);
  val agentsCanvas = new AgentsCanvas( agents, cols, rows )

  contents = new BorderPanel {
    border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
    add(agentsCanvas, BorderPanel.Position.Center)
  }

  def agentUpdate(currRole : SGAgent.SGRole.Value, prevRole : SGAgent.SGRole.Value, pos: (Int, Int)) = {
    val idx = pos._2 * cols + pos._1;

    agents( idx ) = new AgentInfo(currRole, prevRole, pos);
    agentsCanvas.repaint();
  }
}

class AgentsCanvas(val agents: Array[AgentInfo], cols : Int, rows : Int) extends Component {
  val BORDER_X = 10;
  val BORDER_Y = 10;

  val BLUE = new Color(0.0f, 0.0f, 0.7f)
  val GREEN = new Color(0.0f, 0.55f, 0.0f)
  val YELLOW = new Color(1.0f, 0.7f, 0.0f)
  val RED = new Color(0.7f, 0.0f, 0.0f)

  override def paintComponent(g : Graphics2D) {
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size.width, size.height)

    val d = size
    val width = d.width - 2 * BORDER_X
    val height = d.height - 2 * BORDER_Y

    val cellWidth = width / cols
    val cellHeight = height / rows

    for (a <- agents) {
      if (a != null) {
        val x = BORDER_X + a.pos._1 * cellWidth
        val y = BORDER_Y + a.pos._2 * cellHeight

        if (a.prevRole == SGAgent.SGRole.Cooperator && a.currRole == SGAgent.SGRole.Cooperator)
          g.setColor(BLUE)
        else if (a.prevRole == SGAgent.SGRole.Defector && a.currRole == SGAgent.SGRole.Defector)
          g.setColor(RED)
        else if (a.prevRole == SGAgent.SGRole.Defector && a.currRole == SGAgent.SGRole.Cooperator)
          g.setColor(GREEN)
        else if (a.prevRole == SGAgent.SGRole.Cooperator && a.currRole == SGAgent.SGRole.Defector)
          g.setColor(YELLOW)

        val r = new Rectangle2D.Double(x, y, cellWidth, cellHeight)
        g.fill(r)
      }
    }
  }
}