package hac.ui

/**
  * Created by jonathan on 06/12/16.
  */
package wildfire.actors

import wildfire.ui.HACFrontend
import akka.actor.Actor
import akka.actor.Props
import hac.actors.HACAgent

object HACVisualizer {
  def props(agentCount : Int): Props = Props( new HACVisualizer(agentCount) )
}

class HACVisualizer(agentCount : Int) extends Actor {
  val ui = new HACFrontend(agentCount)
  ui.visible = true

  def receive = {
    case HACAgent.AgentChanged(a) => ui.agentUpdate(a);
  }
}