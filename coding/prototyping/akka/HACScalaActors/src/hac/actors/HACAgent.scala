package hac.actors

/**
  * Created by jonathan on 06/12/16.
  */

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import akka.actor.ReceiveTimeout
import hac.actors.HACAgent.{PositionReply, RequestPosition}
import scala.concurrent.duration._
import scala.language.postfixOps

object HACAgent {
  def props( id: Int,
             pos: (Double, Double),
             hero: Boolean,
             visualizer: ActorRef): Props = Props( new HACAgent(id, pos, hero, visualizer) )

  case object Start
  case object RequestPosition

  case class PositionReply(pos: (Double, Double))
  case class AgentChanged(a: HACAgent)
}

class HACAgent( id: Int,
                initPos: (Double, Double),
                hero: Boolean,
                visualizer: ActorRef) extends Actor {

  private var friend: ActorRef = null;
  private var enemy: ActorRef = null;
  private var pos = initPos;

  private var currentFriendPos: (Double, Double) = null;
  private var currentEnemyPos: (Double, Double) = null;

  private var lastUpdate: Long = 0;

  private val SPEED = 0.5;

  def getId() : Int = {
    id
  }

  def isHero() : Boolean = {
    hero
  }

  def getPosition(): (Double, Double) = {
    pos;
  }

  def receive = {
    case fe : (ActorRef, ActorRef) => handleFriendEnemy( fe );
  }

  def awaitStart: Receive = {
    case HACAgent.Start => handleStart();
  }

  def running: Receive = {
    case ReceiveTimeout => handleRunStep();
    case PositionReply(pos: (Double, Double)) => handleReceivedPosition(sender(), pos);
    case RequestPosition => sender() ! HACAgent.PositionReply( this.pos );
  }

  def handleFriendEnemy( fe: (ActorRef, ActorRef)): Unit = {
    friend = fe._1;
    enemy = fe._2;

    context.become( awaitStart );
  }

  def handleStart(): Unit = {
    visualizer ! new HACAgent.AgentChanged(this);
    this.lastUpdate = System.currentTimeMillis();

    context.become( running );
    context.setReceiveTimeout(100 milliseconds)
  }

  def handleRunStep(): Unit = {
    this.friend ! HACAgent.RequestPosition;
    this.enemy ! HACAgent.RequestPosition;

    if (null == this.currentFriendPos || null == this.currentEnemyPos) {
      return;
    }

    val dt = (System.currentTimeMillis() - this.lastUpdate).toDouble / 1000.0;
    val stepWidth = SPEED.toDouble * dt.toDouble;

    val friendPos = this.currentFriendPos;
    val enemyPos = this.currentEnemyPos;
    val halfFriendEnemyDir = new (Double, Double)((enemyPos._1 - friendPos._1) * 0.5, (enemyPos._2 - friendPos._2) * 0.5);
    var targetPos: (Double, Double) = null;

    if (this.hero)
      targetPos = new (Double, Double)(friendPos._1 + halfFriendEnemyDir._1, friendPos._2 + halfFriendEnemyDir._2);
    else
      targetPos = new (Double, Double)(friendPos._1 - halfFriendEnemyDir._1, friendPos._2 - halfFriendEnemyDir._2);

    val targetDir = new (Double, Double)(targetPos._1 - pos._1, targetPos._2 - pos._2);
    val targetDirNorm = this.normalize(targetDir);
    val step = new (Double, Double)(targetDirNorm._1 * stepWidth, targetDirNorm._2 * stepWidth);

    val newPosition = new (Double, Double)(pos._1 + step._1, pos._2 + step._2);

    this.pos = this.clipPosition( newPosition );
    this.lastUpdate = System.currentTimeMillis();

    visualizer ! new HACAgent.AgentChanged(this);
  }

  def normalize(dir : (Double, Double)) : (Double, Double) = {
    val len = Math.sqrt(dir._1*dir._1 + dir._2*dir._2);
    if ( len == 0.0)
      new (Double, Double)(0, 0);
    else
      new (Double, Double)(dir._1 / len, dir._2 / len);
  }

  def clipPosition(pos : (Double, Double)) : (Double, Double) = {
    val x: Double = Math.max( 0.0, Math.min( pos._1, 1.0 ) );
    val y: Double = Math.max( 0.0, Math.min( pos._2, 1.0 ) );

    new (Double, Double)(x, y);
  }

  def handleReceivedPosition(ref: ActorRef, pos: (Double, Double)): Unit = {
    if ( ref == this.friend ) {
      this.currentFriendPos = new (Double, Double)(pos._1, pos._2);
    } else if ( ref == this.enemy ) {
      this.currentEnemyPos = new (Double, Double)(pos._1, pos._2);
    }
  }
}