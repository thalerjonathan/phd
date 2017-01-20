package HAC.agent;

import agent.Agent;
import agent.Message;

/**
 * Created by jonathan on 20/01/17.
 */
public class HACAgent extends Agent<HACMsgType> {

    private HACAgent friend;
    private HACAgent enemy;

    private Vector pos;

    private boolean hero;

    private final static double DISTANCE_PER_TIMEUNIT = 1.0;

    public HACAgent(Vector pos,
                    boolean hero) {
        this.hero = hero;
        this.pos = pos;
    }

    public void setFriend(HACAgent friend) {
        this.friend = friend;
    }

    public void setEnemy(HACAgent enemy) {
        this.enemy = enemy;
    }

    public HACAgent getFriend() {
        return friend;
    }

    public HACAgent getEnemy() {
        return enemy;
    }

    public Vector getPos() {
        return pos;
    }

    public boolean isHero() {
        return hero;
    }

    @Override
    public void receivedMessage(Agent<HACMsgType> sender, Message<HACMsgType> msg) {
    }

    @Override
    public void dt(Double time, Double delta) {
        this.updatePosition(delta);
    }

    private void updatePosition(Double delta) {
        Vector friendPos = this.friend.getPos();
        Vector enemyPos = this.enemy.getPos();
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
}
