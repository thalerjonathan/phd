package wildfire.actors

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

object WindDirection extends Enumeration {
  val NW, N, NE, W, Z, E, SW, S, SE = Value
}

object Wind {
  case class WindChanged(directions: Array[Double])
  case class ChangeDirection(dir: WindDirection.Value)
  
  def props(direction: WindDirection.Value): Props = Props( new Wind(direction) )
}

class Wind(var direction: WindDirection.Value) extends Actor {
  private var cells: Array[ActorRef] = null;
  private var directions: Array[Array[Double]] = Array( 
      Array( 0.5, 0.1875, 0.025, 0.1875, 0.025, 0.025, 0.025, 0.025 ), // NW
      Array( 0.1875, 0.5, 0.1875, 0.025, 0.025, 0.025, 0.025, 0.025 ), // N
      Array( 0.025, 0.1875, 0.5, 0.025, 0.1875, 0.025, 0.025, 0.025 ), // NE
      Array( 0.1875, 0.025, 0.025, 0.5, 0.025, 0.1875, 0.025, 0.025 ), // W
      Array( 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125 ), // Z
      Array( 0.025, 0.025, 0.1875, 0.025, 0.5, 0.025, 0.025, 0.1875 ), // E
      Array( 0.025, 0.025, 0.025, 0.1875, 0.025, 0.5, 0.1875, 0.025 ), // SW
      Array( 0.025, 0.025, 0.025, 0.025, 0.025, 0.1875, 0.5, 0.1875 ), // S
      Array( 0.025, 0.025, 0.025, 0.025, 0.1875, 0.025, 0.1875, 0.5 )  // SE
      ) 
  
  def receive = {
    case cs : Array[ActorRef] => cells = cs; sendDirections();
    case Wind.ChangeDirection(dir) => direction = dir; sendDirections();
  }
  
  def sendDirections(): Unit = {
    cells.map { x => x ! Wind.WindChanged( directions( direction.id ) ) };
  }
}