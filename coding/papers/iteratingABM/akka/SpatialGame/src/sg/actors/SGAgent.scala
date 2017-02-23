package sg.actors

/**
  * Created by jonathan on 06/12/16.
  */

import akka.actor.{Actor, ActorRef, Props}
import sg.actors.SGAgent.{PayoffChanged, RequestAgentInfo, RoleChanged, SGRole}

import scala.language.postfixOps

object SGAgent {
  object SGRole extends Enumeration {
    val Cooperator, Defector = Value;
  }

  def props( role: SGAgent.SGRole.Value,
             pos: (Int, Int)): Props = Props( new SGAgent(role, pos) )

  case object Start
  case object RequestAgentInfo

  case class PayoffChanged(payoffValue : Double, payoffRole : SGAgent.SGRole.Value);
  case class RoleChanged(role : SGAgent.SGRole.Value);

  case class AgentInfo(currRole : SGAgent.SGRole.Value, prevRole : SGAgent.SGRole.Value, pos: (Int, Int), localTime : Int)
}

class SGAgent( initRole: SGAgent.SGRole.Value,
                initPos: (Int, Int)) extends Actor {

  private val B = 1.9
  private val S = 0.0
  private val P = 0.0
  private val R = 1.0

  private var neighbours : Array[ActorRef] = null;
  private var pos = initPos;
  private var localTime = 0;

  private var currRole = initRole;
  private var prevRole = initRole;

  private var localPayoff = .0

  private var bestPayoffValue = .0
  private var bestPayoffState: SGAgent.SGRole.Value = null;

  private var neighbourPayoffCount : Int = 0;
  private var neighbourStateCount : Int = 0;

  def getPos(): (Int, Int) = {
    pos;
  }

  def receive = {
    case ns : Array[ActorRef] => handleNeighbours( ns );
  }

  def awaitStart: Receive = {
    case SGAgent.Start => handleStart();
  }

  def running: Receive = {
    case PayoffChanged(payoffValue : Double, payoffRole : SGAgent.SGRole.Value) => handlePayoff(payoffValue, payoffRole);
    case RoleChanged(role : SGAgent.SGRole.Value) => handleRole(role);
    case RequestAgentInfo => sender() ! new SGAgent.AgentInfo( this.currRole, this.prevRole, this.pos, this.localTime );
  }

  def handleNeighbours(ns: Array[ActorRef]): Unit = {
    neighbours = ns;
    context.become( awaitStart );
  }

  def handleStart(): Unit = {
    this.neighbourPayoffCount = this.neighbours.length;
    this.neighbourStateCount = this.neighbours.length;
    this.broadcastLocalState();

    context.become( running );
  }

  def broadcastLocalState(): Unit = {
    val msg = RoleChanged(this.currRole);

    for (n <- this.neighbours) {
      if ( n != null)
        n ! msg;
    }
  }

  def broadcastLocalPayoff(): Unit = {
    val msg = PayoffChanged(this.localPayoff, this.currRole);

    for (n <- this.neighbours) {
      if ( n != null)
        n ! msg;
    }
  }

  def handlePayoff(payoffValue: Double, payoffRole: _root_.sg.actors.SGAgent.SGRole.Value): Unit = {
    if ( payoffValue > this.bestPayoffValue ) {
      this.bestPayoffValue = payoffValue;
      this.bestPayoffState = payoffRole;
    }

    this.neighbourPayoffCount = this.neighbourPayoffCount - 1;

    if ( 0 == this.neighbourPayoffCount ) {
      this.prevRole = this.currRole;
      this.currRole = this.bestPayoffState;

      this.localPayoff = 0.0;
      this.bestPayoffValue = 0.0;

      this.neighbourPayoffCount = this.neighbours.length;
      this.broadcastLocalState();
    }
  }

  def handleRole(role: _root_.sg.actors.SGAgent.SGRole.Value): Unit = {
    val po = calculatePayoff( this.currRole, role);
    this.localPayoff += po;

    this.neighbourStateCount = this.neighbourStateCount - 1;

    if ( 0 == this.neighbourStateCount ) {
      this.neighbourStateCount = this.neighbours.size;
      this.broadcastLocalPayoff();
    }
  }

  def calculatePayoff(ref: _root_.sg.actors.SGAgent.SGRole.Value, other: _root_.sg.actors.SGAgent.SGRole.Value): Double = {
    if ((SGRole.Defector eq ref) && (SGRole.Defector eq other)) return P
    else if ((SGRole.Cooperator eq ref) && (SGRole.Defector eq other)) return S
    else if ((SGRole.Defector eq ref) && (SGRole.Cooperator eq other)) return B
    // NOTE: SGState.Cooperator == ref && SGState.Cooperator == other
    return R
  }

}