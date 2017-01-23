package agent;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Created by jonathan on 20/01/17.
 */
public abstract class Agent<M extends Comparable<M>> implements Comparable<Agent<M>> {

    private int id;
    private List<MsgPair> inBox;
    private List<MsgPair> outBox;
    private List<Agent<M>> neighbours;

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

    public void addNeighbour(Agent<M> n) {
        if ( null == this.neighbours )
            this.neighbours = new ArrayList<>();

        this.neighbours.add(n);
    }

    public void setNeighbours(List<Agent<M>> ns) {
        this.neighbours = ns;
    }

    public void step(Double time, Double delta) {
        Iterator<MsgPair> iter = this.inBox.iterator();
        while (iter.hasNext()) {
            MsgPair p = iter.next();

            this.receivedMessage(p.agent, p.msg);
        }

        this.inBox.clear();

        this.dt(time, delta);
    }

    public void sendMessageToRandomNeighbour(Message<M> msg) {
        if ( null == this.neighbours )
            return;

        int randIdx = (int) (ThreadLocalRandom.current().nextDouble() * this.neighbours.size());
        Agent randNeigh = this.neighbours.get(randIdx);

        this.sendMessage(msg, randNeigh);
    }

    public void sendMessage(Message<M> msg, Agent<M> receiver) {
        this.outBox.add(new MsgPair(receiver, msg));
    }

    public abstract void receivedMessage(Agent<M> sender, Message<M> msg);

    // HASKELL IS BETTER HERE: cannot include the Dt in the Message-Type M in Java, thus need to split it up into separate functions
    public abstract void dt(Double time, Double delta);

    @Override
    public int compareTo(Agent<M> o) {
        return o.id - this.id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Agent<?> agent = (Agent<?>) o;

        return id == agent.id;
    }

    @Override
    public int hashCode() {
        return id;
    }
}
