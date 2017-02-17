package HAC.agent;

import agent.Agent;
import agent.Message;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by jonathan on 20/01/17.
 */
public class HACAgent extends Agent<HACMsgType, Void> implements Cloneable {

    private Agent friend;
    private Agent enemy;

    private Vector friendPos;
    private Vector enemyPos;

    private Vector pos;

    private boolean hero;

    private final static double DISTANCE_PER_TIMEUNIT = 1.0;

    private final static String POSITIONUPDATE_POS_KEY = "POS";
    private final static Message<HACMsgType> MSG_REQUESTPOSITION = new Message<>(HACMsgType.RequestPosition);

    public HACAgent(Vector pos,
                    boolean hero) {
        this.hero = hero;
        this.pos = pos;
    }

    public Vector getPos() {
        return pos;
    }

    public boolean isHero() {
        return hero;
    }

    public void setFriend(Agent friend) {
        this.friend = friend;
    }

    public void setEnemy(Agent enemy) {
        this.enemy = enemy;
    }

    @Override
    public void start() {

    }

    @Override
    public void receivedMessage(Agent<HACMsgType, Void> sender, Message<HACMsgType> msg, Map<Integer, Void> globalEnv) {
        if (msg.isOfType(HACMsgType.RequestPosition)) {
            this.handleRequestPosition(sender);
        } else if (msg.isOfType( HACMsgType.PositionUpdate)) {
            this.handlePositionUpdate(sender, msg);
        }
    }

    @Override
    public void dt(Double time, Double delta, Map<Integer, Void> globalEnv) {
        this.updatePosition(delta);

        // JAVA IS MORE CONVENIENT: no need for annoying imperative-style fakings
        this.sendMessage(MSG_REQUESTPOSITION, this.friend);
        this.sendMessage(MSG_REQUESTPOSITION, this.enemy);
    }

    @Override
    public HACAgent clone() throws CloneNotSupportedException {
        HACAgent clonedHACAgent = (HACAgent) super.clone();

        // NOTE: no need for deep-copying members

        return clonedHACAgent;
    }

    private void updatePosition(Double delta) {
        // JAVA IS MORE CONVENIENT: because we have nullable aliases, we can check them for being null and no need to utilize a specific type like Maybe
        if ( null == this.friendPos || null == this.enemyPos )
            return;

        Vector targetPos;
        Vector friendEnemyDirection = friendPos.delta(enemyPos).multiply(0.5);

        if (this.hero) {
            targetPos = friendPos.add( friendEnemyDirection );
        } else {
            targetPos = friendPos.sub( friendEnemyDirection );
        }

        Vector targetDir = this.pos.delta(targetPos).norm();
        Vector newPos = this.pos.add(targetDir.multiply(HACAgent.DISTANCE_PER_TIMEUNIT * delta));

        this.pos = newPos.clip( 0.0, 1.0 );
    }

    private void handlePositionUpdate(Agent sender, Message<HACMsgType> msg) {
        // HASKELL IS BETTER HERE: this is not type-safe in Java and not even possible. We can't build statically typed data-constructors like in Haskell
        Vector newPosition = (Vector) msg.getValue(POSITIONUPDATE_POS_KEY);

        if (sender == this.enemy) {
            this.enemyPos = newPosition;
        } else if (sender == this.friend) {
            this.friendPos = newPosition;
        }
    }

    private void handleRequestPosition(Agent sender) {
        Message<HACMsgType> posMsg = new Message<>(HACMsgType.PositionUpdate);
        posMsg.addValue(POSITIONUPDATE_POS_KEY, new Vector(this.pos)); // NOTE: send a new instance!!

        this.sendMessage(posMsg, sender);
    }
}
