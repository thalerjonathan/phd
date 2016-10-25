package wildfire.actors

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

object Wind {
  def props(): Props = Props( new Wind() )
}

class Wind() extends Actor {
  private var cells: Array[ActorRef] = null;
  
  def receive = {
    case cs : Array[ActorRef] => cells = cs; //println( "received cells in " + self );
  }
}