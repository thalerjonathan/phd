package agent;

import java.util.*;

/**
 * Created by jonathan on 20/01/17.
 */
public abstract class Agent<M extends Comparable<M>> implements Comparable<Agent<M>>, Cloneable {

    private int id;
    private List<MsgPair> inBox;
    private List<MsgPair> outBox;

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

    public void step(Double time, Double delta) {
        Iterator<MsgPair> iter = this.inBox.iterator();
        while (iter.hasNext()) {
            MsgPair p = iter.next();

            this.receivedMessage(p.agentId, p.msg);
        }

        this.inBox.clear();

        this.dt(time, delta);
    }

    public Agent clone() throws CloneNotSupportedException {
        Agent clonedAgent = (Agent) super.clone();
        clonedAgent.inBox = new LinkedList<>();
        clonedAgent.outBox =  new LinkedList<>();

        // NOTE: no need to copy the content of the outbox!

        //clonedAgent.inBox.addAll( this.inBox );
        //clonedAgent.outBox.addAll( this.outBox );

        return clonedAgent;
    }

    public void sendMessage(Message<M> msg, int receiverId) {
        this.outBox.add(new MsgPair(receiverId, msg));
    }

    public abstract void receivedMessage(int senderId, Message<M> msg);

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
