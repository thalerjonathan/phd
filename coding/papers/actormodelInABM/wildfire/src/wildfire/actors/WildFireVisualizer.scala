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
    case FireCell.StartBurning(coords) => ui.cellStartBurning(coords);
    case FireCell.StopBurning(coords) => ui.cellStopBurning(coords);
    case FireCell.UpdateFurel(coords, fuel) => ui.fuelChanged(coords, fuel);
  }
}