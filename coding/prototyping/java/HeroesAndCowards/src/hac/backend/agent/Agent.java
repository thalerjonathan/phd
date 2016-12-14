package hac.backend.agent;

import hac.backend.simulation.WorldType;

import java.util.Random;

/**
 * Created by jonathan on 05/12/16.
 */
public class Agent implements Comparable<Agent> {

    private int id;
    private Vector pos;
    private Agent friend;
    private Agent enemy;
    private boolean hero;

    public final static double SPEED = 1.0;

    public Agent(int id) {
        this.id = id;
    }

    public Agent(Agent cpy) {
        this.id = cpy.id;
        this.pos = new Vector( cpy.pos );
        this.enemy = cpy.enemy;
        this.friend = cpy.friend;
        this.hero = cpy.hero;
    }

    public int getId() {
        return id;
    }

    public Agent getFriend() {
        return friend;
    }

    public Agent getEnemy() {
        return enemy;
    }

    public boolean isHero() {
        return hero;
    }

    public void setHero(boolean hero) {
        this.hero = hero;
    }

    public Vector getPos() {
        return pos;
    }

    public void setPos(Vector pos) {
        this.pos = pos;
    }

    public void setFriend(Agent friend) {
        this.friend = friend;
    }

    public void setEnemy(Agent enemy) {
        this.enemy = enemy;
    }

    public Agent stepImmutable(double dt, WorldType wt, Random r) {
        Agent a = new Agent( this );
        a.step(dt, wt, r);
        return a;
    }

    public void step(double dt, WorldType wt, Random r) {
        Vector friendPos = this.friend.getPos();
        Vector enemyPos = this.enemy.getPos();
        Vector friendEnemyDirection = friendPos.delta(enemyPos);
        Vector targetPos;

        if (this.hero) {
            targetPos = friendPos.add( friendEnemyDirection.multiply( 0.5 ) );
        } else {
            targetPos = friendPos.sub( friendEnemyDirection.multiply( 0.5 ) );
        }

        Vector targetDir = this.pos.delta( targetPos ).norm();
        double noise = r.nextDouble();
        Vector newPos = this.pos.add( targetDir.multiply( Agent.SPEED * dt * noise ) );

        Vector newWtPos;

        if ( WorldType.BORDER == wt ) {
            newWtPos = newPos.clip();
        } else if ( WorldType.WRAPPING == wt ) {
            newWtPos = newPos.wrap();
        } else {
            newWtPos = newPos;
        }

        this.pos = newWtPos;
    }

    @Override
    public int compareTo(Agent o) {
        return this.id - o.id;
    }
}
