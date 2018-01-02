package wildfire.actors

import wildfire.ui.WildFireUI

import akka.actor.Actor
import akka.actor.Props

object WildFireVisualizer {
  def props(xDim : Int, yDim : Int): Props = Props( new WildFireVisualizer(xDim, yDim) )
}

class WildFireVisualizer(xDim : Int, yDim : Int) extends Actor {
  val ui = new WildFireUI(xDim, yDim)
  ui.visible = true
  
  def receive = {
    case Cell.ChangedState(coords, state) => ui.cellChangedState(coords, state);
    case Cell.BecomesAlive(coords, fuel) => ui.cellBecomesAlive(coords, fuel);
  }
}