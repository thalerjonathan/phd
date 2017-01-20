package agent;

import java.util.*;

/**
 * Created by jonathan on 20/01/17.
 */
public abstract class Agent<M extends Comparable<M>> implements Comparable<Agent<M>> {

    private int id;
    private List<MsgPair> msgBox;

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

    public void step(Double time, Double delta) {
        Iterator<MsgPair> iter = this.msgBox.iterator();
        while (iter.hasNext()) {
            MsgPair p = iter.next();

            this.receivedMessage(p.sender, p.msg);
        }

        this.msgBox.clear();

        this.dt(time, delta);
    }

    public void sendMessage(Message<M> msg, Agent<M> receiver) {
        receiver.msgBox.add(new MsgPair(this, msg));
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
