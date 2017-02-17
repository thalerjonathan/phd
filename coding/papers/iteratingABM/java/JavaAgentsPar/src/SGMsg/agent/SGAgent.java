package SGMsg.agent;

import agent.Agent;
import agent.Message;
import utils.Cell;
import utils.Pair;

import java.util.Map;

/**
 * Created by jonathan on 23/01/17.
 */
public class SGAgent extends Agent<SGMsgType, Void> {

    private final static double B = 1.9;
    private final static double S = 0.0;
    private final static double P = 0.0;
    private final static double R = 1.0;

    private final static String NEIGHBOURSTATE_KEY = "STATE";
    private final static String NEIGHBOURPAYOFF_STATE_KEY = "POSTATE";
    private final static String NEIGHBOURPAYOFF_VALUE_KEY = "POVALUE";

    public enum SGState {
        Defector,
        Cooperator
    }

    private Cell c;

    private SGState currState;
    private SGState prevState;

    private double localPayoff;

    private double bestPayoffValue;
    private SGState bestPayoffState;

    private int neighbourFlags;

    public SGAgent(SGState state, Cell c) {
        this.currState = state;
        this.prevState  = state;

        this.localPayoff = 0.0;

        this.bestPayoffValue = 0.0;
        this.bestPayoffState = state;

        this.c = c;
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
    public void receivedMessage(Agent<SGMsgType, Void> sender,
                                Message<SGMsgType> msg,
                                Map<Integer, Void> globalEnv) {
        if ( msg.isOfType(SGMsgType.NeighbourState)) {
            this.receivedStateMessage(msg);

        } else if ( msg.isOfType(SGMsgType.NeighbourPayoff)) {
            this.receivedPayoffMessage(msg);
        }
    }

    @Override
    public void start() {
        this.broadcastLocalState();
    }


    @Override
    public void dt(Double time, Double delta, Map<Integer, Void> globalEnv) {

    }

    private void receivedPayoffMessage(Message<SGMsgType> msg) {
        double payoffValue = (Double) msg.getValue( NEIGHBOURPAYOFF_VALUE_KEY );
        SGState payoffState = (SGState) msg.getValue( NEIGHBOURPAYOFF_STATE_KEY );

        if ( payoffValue > this.bestPayoffValue ) {
            this.bestPayoffValue = payoffValue;
            this.bestPayoffState = payoffState;
        }

        this.neighbourFlags--;

        if (0 == this.neighbourFlags) {
            this.prevState = this.currState;
            this.currState = this.bestPayoffState;
            this.localPayoff = 0.0;
            this.bestPayoffValue = 0.0;

            this.broadcastLocalState();
        }
    }

    private void receivedStateMessage(Message<SGMsgType> msg) {
        SGState neighbourState = (SGState) msg.getValue( NEIGHBOURSTATE_KEY );
        double po = calculatePayoff( this.currState, neighbourState );
        this.localPayoff += po;

        this.neighbourFlags--;

        if (0 == this.neighbourFlags)
            this.broadcastLocalPayoff();
    }

    private void broadcastLocalPayoff() {
        this.neighbourFlags = this.getNeighbours().size();

        Message<SGMsgType> myPayoff = new Message<>(SGMsgType.NeighbourPayoff);
        myPayoff.addValue( NEIGHBOURPAYOFF_VALUE_KEY, this.localPayoff );
        myPayoff.addValue( NEIGHBOURPAYOFF_STATE_KEY, this.currState );
        this.broadCastToNeighbours(myPayoff);
    }

    private void broadcastLocalState() {
        this.neighbourFlags = this.getNeighbours().size();

        Message<SGMsgType> myState = new Message<>(SGMsgType.NeighbourState);
        myState.addValue( NEIGHBOURSTATE_KEY, this.currState );
        this.broadCastToNeighbours(myState);
    }

    private double calculatePayoff(SGState ref, SGState other) {
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
