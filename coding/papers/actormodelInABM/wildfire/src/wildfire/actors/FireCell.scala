package wildfire.actors

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.actorRef2Scala

object FireCell {
  def props(coords: Tuple2[Int, Int], initFuel: Double, visualizer: ActorRef): Props = Props( new FireCell(coords, initFuel, visualizer) )

  case object Ignite
  
  case class StartBurning(coords: Tuple2[Int, Int])
  case class StopBurning(coords: Tuple2[Int, Int])
  case class UpdateFurel(coords: Tuple2[Int, Int], fuel: Double)
}

class FireCell(coords: Tuple2[Int,Int], initFuel: Double, visualizer: ActorRef) extends Actor {
  private var burning: Boolean = false;
  private var fuel = initFuel;
  private var neighbours: Array[ActorRef] = null;

  def receive = {
    case ns : Array[ActorRef] => neighbours = ns; visualizer ! new FireCell.UpdateFurel(coords, fuel);
    case FireCell.Ignite => burning = true; visualizer ! new FireCell.StartBurning(coords);
  }
}