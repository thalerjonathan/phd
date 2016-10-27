import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.actorRef2Scala
import wildfire.actors.Cell
import wildfire.actors.WildFireVisualizer
import wildfire.actors.Wind
import wildfire.actors.WindDirection

object WildFireMain {
  def main(args: Array[String]): Unit = {
    val xDim = 500;
    val yDim = 500;
    
    val system = ActorSystem("HelloSystem");
    val visualizer = system.actorOf( WildFireVisualizer.props(xDim, yDim), name = "WildFireVisualizer" );
    val wind = system.actorOf( Wind.props( WindDirection.Z ), name = "Wind" );
    
    val neighbourhood = new Array[Tuple2[Int, Int]]( 8 );
    neighbourhood(0) = new Tuple2(-1,-1);
    neighbourhood(1) = new Tuple2(0,-1);
    neighbourhood(2) = new Tuple2(+1,-1);
    neighbourhood(3) = new Tuple2(-1,0);
    neighbourhood(4) = new Tuple2(+1,0);
    neighbourhood(5) = new Tuple2(-1,+1);
    neighbourhood(6) = new Tuple2(0,+1);
    neighbourhood(7) = new Tuple2(+1,+1);
     
    val cells = Array.ofDim[ActorRef]( xDim, yDim);
    
    for( x <- 0 to xDim - 1 ) {
      cells(x) = new Array[ActorRef]( xDim );
        
      for ( y <- 0 to yDim - 1 ) {
         cells(x)(y) = system.actorOf( Cell.props( new Tuple2(x, y), Math.random(), visualizer ), name = "fireCell_" + x + "_" + y + "_" );
      }
    }
    
    for( x <- 0 to xDim - 1 ) {
      for ( y <- 0 to yDim - 1 ) {
        val c = cells(x)(y);
        val ns = neighbourhood.map{ case (xn: Int, yn : Int) => 
          val xCoord = x + xn;
          val yCoord = y + yn;
          
          if ( xCoord < 0 || yCoord < 0 || xCoord >= xDim || yCoord >= yDim )
            null;
          else
            cells(xCoord)(yCoord);
        }
        
        c ! ns;
      }
    }

    wind ! cells.flatMap { x => x }; 
    
    Thread.sleep(1000);
    
    val centerX = xDim / 2;
    val centerY = yDim / 2;
    
    cells(centerX)(centerY) ! Cell.Ignite
    
    /*
    Thread.sleep(1000);
    wind ! Wind.ChangeDirection(WindDirection.N);
    Thread.sleep(1000);
    wind ! Wind.ChangeDirection(WindDirection.S);
    Thread.sleep(1000);
    wind ! Wind.ChangeDirection(WindDirection.W);
    Thread.sleep(1000);
    wind ! Wind.ChangeDirection(WindDirection.E);
    * */
  }
}