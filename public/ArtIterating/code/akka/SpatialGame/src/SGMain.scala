import akka.actor.{ActorRef, ActorSystem}
import sg.actors.SGAgent
import sg.actors.SGAgent.SGRole
import sg.ui.SGVisualizer

/**
  * Created by jonathan on 06/12/16.
  */

object SGMain {

  val rows = 99;
  val cols = 99;

  def main(args: Array[String]): Unit = {
    val agentCount = rows * cols;

    val system = ActorSystem("SGMainSystem");

    val agents = Array.ofDim[ActorRef]( agentCount );
    val agentPos = Array.ofDim[(Int, Int)]( agentCount );

    var idx = 0;

    val halfCols = (cols / 2.0).toInt
    val halfRows = (rows / 2.0).toInt

    for( y <- 0 to rows - 1 ) {
      for( x <- 0 to cols - 1 ) {
        var role = SGRole.Cooperator;
        val pos = new Tuple2[Int, Int](x, y);

        if ( halfCols == x && halfRows == y)
          role = SGRole.Defector;

        idx = y * cols + x;

        agents(idx) = system.actorOf(SGAgent.props(role, pos));
        agentPos(idx) = (x, y);
      }
    }

    for( i <- 0 to agentCount - 1 ) {
      val a = agentPos( i );
      val ns = getNeighbours( a, agents, agentPos );

      agents(i) ! ns
    }

    Thread.sleep(1000);

    for( i <- 0 to agentCount - 1 ) {
      val a = agents( i );
      a ! SGAgent.Start
    }

    val visualizer = system.actorOf( SGVisualizer.props(agents, cols, rows), name = "SGVisualizer" );
    visualizer ! SGVisualizer.Start
  }

  def getNeighbours(aPos: (Int, Int), agents: Array[ActorRef], agentPos: Array[(Int, Int)]): Array[ActorRef] = {
    val nCells = neighbourhood(aPos);
    var i = 0;
    val neighbours = Array.ofDim[ActorRef](9);

    for (n <- agentPos) {
      for (nc <- nCells) {
        if (n == nc) {
          val idx = nc._2 * cols + nc._1;

          neighbours( i ) = agents( idx );
          i = i + 1;
        }
      }
    }

    neighbours
  }

  def neighbourhood(a: (Int, Int)): Array[(Int, Int)] = {
    var n = Array.ofDim[(Int, Int)]( 9 );
    val x = a._1;
    val y = a._2;

    n(0) = (x - 1,  y - 1);
    n(1) = (x,      y - 1);
    n(2) = (x + 1,  y - 1);

    n(3) = (x - 1,  y);
    n(4) = (x,      y);
    n(5) = (x + 1,  y);

    n(6) = (x - 1,  y + 1);
    n(7) = (x,      y + 1);
    n(8) = (x + 1,  y + 1);
    n
  }
}