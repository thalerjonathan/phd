package wildfire.actors

import akka.actor.Actor
import akka.actor._
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import akka.actor.ReceiveTimeout
import scala.concurrent.duration._
import akka.pattern.gracefulStop

object CellState extends Enumeration {
  val LIVING, BURNING, DEAD = Value
}

object Cell {
  def props(coords: Tuple2[Int, Int], initFuel: Double, visualizer: ActorRef): Props = Props( new Cell(coords, initFuel, visualizer) )

  case object Ignite
  
  case class ChangedState(coords: Tuple2[Int, Int], state: CellState.Value )
  case class BecomesAlive(coords: Tuple2[Int, Int], fuel: Double)
}

class Cell(coords: Tuple2[Int,Int], initFuel: Double, visualizer: ActorRef) extends Actor {
  private var fuel = initFuel;
  private var neighbours: Array[ActorRef] = null;
  private var windDirections: Array[Double] = null;
  
  def living: Receive = {
    case Wind.WindChanged(directions) => windDirections = directions;
    case Cell.Ignite => handleIgnition();
  }
  
  def burning: Receive = {
    case Wind.WindChanged(directions) => windDirections = directions;
    case ReceiveTimeout => handleBurningTime();
    case _ => // NOTE: ignore Ignite-Messages
  }
  
  def receive = {
    case ns : Array[ActorRef] => handleNeighbours( ns );
  }
    
  def handleNeighbours( ns: Array[ActorRef]): Unit = {
    neighbours = ns;
    visualizer ! new Cell.BecomesAlive(coords, fuel);
    context.become( living );
  }
  
  def handleIgnition(): Unit = {
    context.become( burning );
    
    visualizer ! new Cell.ChangedState(coords, CellState.BURNING);
    igniteNeighbour();
    context.setReceiveTimeout(100 milliseconds)
  }
  
  def handleBurningTime(): Unit = {
    fuel -= 0.2;
    
    if ( fuel <= 0.0 ) {
      fuel = 0.0;
      visualizer ! new Cell.ChangedState(coords, CellState.DEAD);
      context.setReceiveTimeout(Duration.Undefined);
      self ! PoisonPill
      
    } else {
      igniteNeighbour();
    }
  }
  
  def igniteNeighbour(): Unit = {
    val randomNeighbour = getRandomNeighbour();
    if ( randomNeighbour != null )
      randomNeighbour ! Cell.Ignite
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