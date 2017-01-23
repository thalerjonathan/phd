package SGMsg.agent;

import agent.Agent;
import agent.Message;
import utils.Cell;

/**
 * Created by jonathan on 23/01/17.
 */
public class SGAgent extends Agent<SGMsgType, Void> {

    private final static double B = 1.9;
    private final static double S = 0.0;
    private final static double P = 0.0;
    private final static double R = 1.0;

    private final static String NEIGHBOURACTION_KEY = "NACT";
    private final static String NEIGHBOURPAYOFF_STATE_KEY = "POSTATE";
    private final static String NEIGHBOURPAYOFF_VALUE_KEY = "POVALUE";

    public enum SGState {
        Defector,
        Cooperator
    }

    private Cell c;

    private SGState currState;
    private SGState prevState;

    private double sumPayoff;

    private double maxPayoffValue;
    private SGState maxPayoffState;

    public SGAgent(SGState state, Cell c) {
        this.currState = state;
        this.prevState  = state;

        this.sumPayoff = 0.0;

        this.maxPayoffValue = 0.0;
        this.maxPayoffState = state;

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
                                Void env) {
        if ( msg.isOfType(SGMsgType.NeighbourAction)) {
            SGState neighbourAction = (SGState) msg.getValue( NEIGHBOURACTION_KEY );
            double po = calculatePayoff( this.currState, neighbourAction );
            this.sumPayoff += po;

        } else if ( msg.isOfType(SGMsgType.NeighbourPayoff)) {
            double payoffValue = (Double) msg.getValue( NEIGHBOURPAYOFF_VALUE_KEY );
            SGState payoffState = (SGState) msg.getValue( NEIGHBOURPAYOFF_STATE_KEY );

            if ( payoffValue > this.maxPayoffValue ) {
                this.maxPayoffValue = payoffValue;
                this.maxPayoffState = payoffState;
            }
        }
    }

    @Override
    public void dt(Double time, Double delta, Void env) {
        Message<SGMsgType> myPayoff = new Message<>(SGMsgType.NeighbourPayoff);
        myPayoff.addValue( NEIGHBOURPAYOFF_VALUE_KEY, this.sumPayoff );
        myPayoff.addValue( NEIGHBOURPAYOFF_STATE_KEY, this.currState );
        this.broadCastToNeighbours( myPayoff );

        Message<SGMsgType> myAction = new Message<>(SGMsgType.NeighbourAction);
        myAction.addValue( NEIGHBOURACTION_KEY, this.maxPayoffState );
        this.broadCastToNeighbours( myAction );

        this.prevState = this.currState;
        this.currState = this.maxPayoffState;
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
