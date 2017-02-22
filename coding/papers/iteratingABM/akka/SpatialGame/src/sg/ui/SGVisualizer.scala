package sg.ui

/**
  * Created by jonathan on 06/12/16.
  */
import akka.actor.{Actor, ActorRef, Props}
import sg.actors.HACAgent

import scala.concurrent.duration._

object SGVisualizer {
  def props(agents : Array[ActorRef]): Props = Props( new HACVisualizer(agents) )

  case object Start
}

class SGVisualizer(agents : Array[ActorRef]) extends Actor {
  val ui = new HACFrontend(agents.length)

  ui.visible = true

  def receive = {
    case HACVisualizer.Start => handleStart();
  }

  def handleStart(): Unit = {
    context.become( run );
    context.setReceiveTimeout(10 milliseconds);
  }

  def run: Receive = {
    case HACAgent.AgentInfo(id, pos, hero, localTime) => handleInfo(id, pos, hero, localTime);
    case ReceiveTimeout => requestAllInfos();
  }

  def handleInfo(id : Int, pos: (Double, Double), hero : Boolean, localTime : Int): Unit = {
    if ( localTime >= 500 )
      context.stop(self)

    ui.agentUpdate(id, pos, hero);
  }

  def requestAllInfos() : Unit = {
    for( i <- 0 to agents.length - 1 ) {
      val a = agents( i );
      a ! HACAgent.RequestAgentInfo
    }
  }
}