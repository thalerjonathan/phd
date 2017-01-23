package agent;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Created by jonathan on 20/01/17.
 */
public abstract class Agent<M extends Comparable<M>> implements Comparable<Agent<M>> {

    private int id;
    private List<MsgPair> msgBox;
    private List<Agent<M>> neighbours;

    private static int NEXT_ID = 0;

    private class MsgPair {
        public MsgPair(Agent<M> s, Message<M> m) {
            this.sender = s;
            this.msg = m;
        }

        public Agent<M> sender;
        public Message<M> msg;
    }

    public Agent() {
        this(NEXT_ID++);
    }

    public Agent(int id) {
        this.id = id;
        this.msgBox = new LinkedList<>();
    }

    public int getId() {
        return this.id;
    }

    public void addNeighbour(Agent<M> n) {
        if ( null == this.neighbours )
            this.neighbours = new ArrayList<>();

        this.neighbours.add(n);
    }

    public void setNeighbours(List<Agent<M>> ns) {
        this.neighbours = ns;
    }

    public void step(double time, double delta) {
        this.consumeMessages();

        this.dt(time, delta);
    }

    private void consumeMessages() {
        MsgPair p;

        synchronized (this.msgBox) {
            if ( this.msgBox.size() > 0 ) {
                p = this.msgBox.remove(0);
            } else {
                return;
            }
        }

        this.receivedMessage(p.sender, p.msg);

        consumeMessages();
    }

    public void broadCastToNeighbours(Message<M> msg) {
        if ( null == this.neighbours )
            return;

        for (Agent<M> n : this.neighbours ) {
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

    public void sendMessage(Message<M> msg, Agent<M> receiver) {
        synchronized (receiver.msgBox) {
            receiver.msgBox.add(new MsgPair(this, msg));
        }
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
