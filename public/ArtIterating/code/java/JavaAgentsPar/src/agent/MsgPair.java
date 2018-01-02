package agent;

/**
 * Created by jonathan on 20/01/17.
 */
public class MsgPair<M extends Comparable<M>> {
    public MsgPair(Agent a, Message<M> m) {
        this.agent = a;
        this.msg = m;
    }

    public Agent agent;
    public Message<M> msg;
}
