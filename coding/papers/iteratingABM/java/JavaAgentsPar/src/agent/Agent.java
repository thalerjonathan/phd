package agent;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Created by jonathan on 20/01/17.
 */
public abstract class Agent<M extends Comparable<M>, E> implements Comparable<Agent<M, E>> {

    private int id;
    private List<MsgPair> inBox;
    private List<MsgPair> outBox;
    private List<Agent<M, E>> neighbours;

    protected E localEnv;

    private static int NEXT_ID = 0;

    public Agent() {
        this(NEXT_ID++);
    }

    public Agent(int id) {
        this.id = id;
        this.inBox = new LinkedList<>();
        this.outBox = new LinkedList<>();
    }

    public int getId() {
        return this.id;
    }

    List<MsgPair> getInBox() {
        return inBox;
    }

    List<MsgPair> getOutBox() {
        return outBox;
    }

    public void addNeighbour(Agent<M, E> n) {
        if ( null == this.neighbours )
            this.neighbours = new ArrayList<>();

        this.neighbours.add(n);
    }

    public void setNeighbours(List<Agent<M, E>> ns) {
        this.neighbours = ns;
    }

    public List<Agent<M, E>> getNeighbours() {
        return this.neighbours;
    }

    E getLocalEnv() {
        return this.localEnv;
    }

    public void step(Double time, Double delta, Map<Integer, E> globalEnv) {
        Iterator<MsgPair> iter = this.inBox.iterator();
        while (iter.hasNext()) {
            MsgPair p = iter.next();

            this.receivedMessage(p.agent, p.msg, globalEnv);
        }

        this.inBox.clear();

        this.dt(time, delta, globalEnv);
    }

    public void broadCastToNeighbours(Message<M> msg) {
        if ( null == this.neighbours )
            return;

        for (Agent<M, E> n : this.neighbours ) {
            this.sendMessage(msg, n);
        }
    }

    public void sendMessageToRandomNeighbour(Message<M> msg) {
        if ( null == this.neighbours )
            return;

        int randIdx = (int) (ThreadLocalRandom.current().nextDouble() * this.neighbours.size());
        Agent randNeigh = this.neighbours.get(randIdx);

        this.sendMessage(msg, randNeigh);
    }

    public void sendMessage(Message<M> msg, Agent<M, E> receiver) {
        this.outBox.add(new MsgPair(receiver, msg));
    }

    public abstract void receivedMessage(Agent<M, E> sender, Message<M> msg, Map<Integer, E> globalEnv);

    // HASKELL IS BETTER HERE: cannot include the Dt in the Message-Type M in Java, thus need to split it up into separate functions
    public abstract void dt(Double time, Double delta, Map<Integer, E> globalEnv);

    @Override
    public int compareTo(Agent<M, E> o) {
        return o.id - this.id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Agent<?,?> agent = (Agent<?,?>) o;

        return id == agent.id;
    }

    @Override
    public int hashCode() {
        return id;
    }
}
