package sg.ui

/**
  * Created by jonathan on 06/12/16.
  */
import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}
import sg.actors.SGAgent

import scala.concurrent.duration._

object SGVisualizer {
  def props(agents : Array[ActorRef], cols : Int, rows : Int): Props = Props( new SGVisualizer(agents, cols, rows) )

  case object Start
}

class SGVisualizer(agents : Array[ActorRef], cols : Int, rows : Int) extends Actor {
  val ui = new SGFrontend(cols, rows)

  ui.visible = true

  def receive = {
    case SGVisualizer.Start => handleStart();
  }

  def handleStart(): Unit = {
    context.become( run );
    context.setReceiveTimeout(10 milliseconds);
  }

  def run: Receive = {
    case SGAgent.AgentInfo(currRole, prevRole, pos, localTime) => handleInfo(currRole, prevRole, pos, localTime);
    case ReceiveTimeout => requestAllInfos();
  }

  def handleInfo(currRole : SGAgent.SGRole.Value, prevRole : SGAgent.SGRole.Value, pos: (Int, Int), localTime : Int): Unit = {
    if ( localTime >= (2*217) )
      context.stop(self)

    ui.agentUpdate(currRole, prevRole, pos);
  }

  def requestAllInfos() : Unit = {
    for( i <- 0 to agents.length - 1 ) {
      val a = agents( i );
      a ! SGAgent.RequestAgentInfo
    }
  }
}