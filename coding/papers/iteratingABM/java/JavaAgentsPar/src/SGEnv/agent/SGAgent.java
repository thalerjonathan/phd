package SGEnv.agent;

import agent.Agent;
import agent.Message;
import utils.Cell;
import utils.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by jonathan on 23/01/17.
 */
public class SGAgent extends Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>> {

    private final static double B = 1.9;
    private final static double S = 0.0;
    private final static double P = 0.0;
    private final static double R = 1.0;

    public enum SGState {
        Defector,
        Cooperator
    }

    private Cell c;

    private SGState currState;
    private SGState prevState;

    public SGAgent(SGState state, Cell c) {
        this.currState = state;
        this.prevState  = state;

        this.c = c;

        this.localEnv = new Pair<>(0.0, this.currState);
    }

    public SGState getCurrState() {
        return currState;
    }

    public SGState getPrevState() {
        return prevState;
    }

    public Cell getCell() {
        return c;
    }

    @Override
    public void start() {

    }

    @Override
    public void receivedMessage(Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>> sender,
                                Message<Message.NoMsg> msg,
                                Map<Integer, Pair<Double, SGState>> globalEnv) {

    }


    @Override
    public void dt(Double time, Double delta, Map<Integer, Pair<Double, SGAgent.SGState>> globalEnv) {
        double localPayoff = 0.0;

        List<Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>>> ns = this.getNeighbours();

        for ( Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>> a : ns ) {
            Pair<Double, SGAgent.SGState> np = globalEnv.get(a.getId());
            localPayoff += SGAgent.calculatePayoff( this.currState, np.r );
        }

        // NOTE: must not modify global environment, change the local one
        this.localEnv = new Pair<>(localPayoff, this.currState);

        SGAgent.SGState bestPayoffState = this.localEnv.r;
        double bestPayoffValue = this.localEnv.l;

        for ( Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>> a : ns ) {
            Pair<Double, SGAgent.SGState> np = globalEnv.get(a.getId());
            if (np.l > bestPayoffValue) {
                bestPayoffState = np.r;
                bestPayoffValue = np.l;
            }
        }

        this.prevState = this.currState;
        this.currState = bestPayoffState;
    }

    private static double calculatePayoff(SGState ref, SGState other) {
        if ( SGState.Defector == ref && SGState.Defector == other ) {
            return P;
        } else if ( SGState.Cooperator == ref && SGState.Defector == other ) {
            return S;
        } else if ( SGState.Defector == ref && SGState.Cooperator == other ) {
            return B;
        }

        // NOTE: SGState.Cooperator == ref && SGState.Cooperator == other
        return R;
    }
}
