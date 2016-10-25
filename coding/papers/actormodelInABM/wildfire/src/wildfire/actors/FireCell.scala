package wildfire.actors

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import akka.actor.ReceiveTimeout
import scala.concurrent.duration._

object CellState extends Enumeration {
  val LIVING, BURNING, DEAD = Value
}

object FireCell {
  def props(coords: Tuple2[Int, Int], initFuel: Double, visualizer: ActorRef): Props = Props( new FireCell(coords, initFuel, visualizer) )

  case object Ignite
  
  case class StartBurning(coords: Tuple2[Int, Int])
  case class StopBurning(coords: Tuple2[Int, Int])
  case class UpdateFurel(coords: Tuple2[Int, Int], fuel: Double)
}

class FireCell(coords: Tuple2[Int,Int], initFuel: Double, visualizer: ActorRef) extends Actor {
  private var state: CellState.Value = CellState.LIVING;
  private var fuel = initFuel;
  private var neighbours: Array[ActorRef] = null;
  private var windDirections: Array[Double] = null;
  
  def receive = {
    case ns : Array[ActorRef] => neighbours = ns; visualizer ! new FireCell.UpdateFurel(coords, fuel);
    case Wind.WindChanged(directions) => windDirections = directions;
    case FireCell.Ignite => handleIgnition();
    case ReceiveTimeout => handleBurningTime()
  }
  
  def handleIgnition(): Unit = {
    if ( CellState.LIVING != state )
      return;
    
    state = CellState.BURNING;
    visualizer ! new FireCell.StartBurning(coords);
    igniteNeighbour();
    context.setReceiveTimeout(100 milliseconds)
  }
  
  def handleBurningTime(): Unit = {
    fuel -= 0.25;
    
    if ( fuel <= 0.0 ) {
      fuel = 0.0;
      state = CellState.DEAD; 
      visualizer ! new FireCell.StopBurning(coords); 
      context.setReceiveTimeout(Duration.Undefined);
      
    } else {
      igniteNeighbour();
    }
  }
  
  def igniteNeighbour(): Unit = {
    val randomNeighbour = getRandomNeighbour();
    if ( randomNeighbour != null )
      randomNeighbour ! FireCell.Ignite
  }
  
  def getRandomNeighbour(): ActorRef = {
    val r = Math.random();
    var sum = 0.0;
    
    for( i <- 0 to windDirections.length - 1 ) {
      sum += windDirections(i);
      if ( sum >= r )
        return neighbours( i );
    }
    
    return null;
  }
}