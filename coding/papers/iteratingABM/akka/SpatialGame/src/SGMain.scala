import akka.actor.{ActorRef, ActorSystem}

/**
  * Created by jonathan on 06/12/16.
  */

object SGMain {
  def main(args: Array[String]): Unit = {
    val heroesDistribution = 0.25;
    val agentCount = 100000;

    val system = ActorSystem("SGMainSystem");

    val agents = Array.ofDim[ActorRef]( agentCount );

    for( i <- 0 to agentCount - 1 ) {
      val idx = i;
      val pos = new Tuple2[Double, Double](Math.random(), Math.random());
      val hero = Math.random() <= heroesDistribution;

      agents(i) = system.actorOf( HACAgent.props( idx, pos, hero ) );
    }

    for( i <- 0 to agentCount - 1 ) {
      val a = agents( i );

      val friend = drawRandomIgnoring(agents, Array(a));
      val enemy = drawRandomIgnoring(agents, Array(a, friend));

      val fe = new Tuple2[ActorRef, ActorRef]( friend, enemy );
      a ! fe
    }

    Thread.sleep(1000);

    for( i <- 0 to agentCount - 1 ) {
      val a = agents( i );
      a ! HACAgent.Start
    }

    Thread.sleep(1000);

    val visualizer = system.actorOf( HACVisualizer.props(agents), name = "HACVisualizer" );
    visualizer ! HACVisualizer.Start
  }

  def drawRandomIgnoring(as : Array[ActorRef], is : Array[ActorRef]) : ActorRef = {
    val randIdx = Math.random() * as.length;
    val randElem = as(randIdx.toInt);

    if (is.exists(p => p == randElem) )
      drawRandomIgnoring(as, is);
    else
      randElem;
  }
}