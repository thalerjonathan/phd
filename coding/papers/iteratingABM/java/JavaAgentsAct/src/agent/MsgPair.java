package agent;

/**
 * Created by jonathan on 20/01/17.
 */
public class MsgPair<M extends Comparable<M>> {
    public MsgPair(int aid, Message<M> m) {
        this.agentId = aid;
        this.msg = m;
    }

    public int agentId;
    public Message<M> msg;
}
