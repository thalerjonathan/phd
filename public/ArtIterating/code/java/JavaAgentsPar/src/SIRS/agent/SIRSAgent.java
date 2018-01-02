package SIRS.agent;

import agent.Agent;
import agent.Message;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Created by jonathan on 23/01/17.
 */
public class SIRSAgent extends Agent<SIRSMsgType, Void> {

    private double durationInState;
    private SIRSState state;
    private Cell c;

    public SIRSAgent(SIRSState state, Cell c) {
        this.state = state;
        this.durationInState = 0;
        this.c = c;
    }

    private final static int DURATION_INFECTED = 7;
    private final static int DURATION_IMMUNE = 14;

    private final static double INFECTION_PROBABILITY = 0.3;

    private final static Message<SIRSMsgType> MSG_CONTACT = new Message<>(SIRSMsgType.ContactInfected);

    public enum SIRSState {
        Susceptible,
        Infected,
        Recovered
    }


    public SIRSState getState() {
        return state;
    }

    public Cell getCell() {
        return c;
    }

    @Override
    public void start() {

    }

    @Override
    public void receivedMessage(Agent<SIRSMsgType, Void> sender, Message<SIRSMsgType> msg, Map<Integer, Void> globalEnv) {
        if (msg.equals( MSG_CONTACT ) )
            this.contactWithInfected();
    }

    @Override
    public void dt(Double time, Double delta, Map<Integer, Void> globalEnv) {
        this.durationInState += delta;

        if (SIRSState.Recovered == this.state)
            this.handleRecoveredAgent();
        else if ( SIRSState.Infected == this.state)
            this.handleInfectedAgent();
    }

    private void contactWithInfected() {
        if ( SIRSState.Infected == this.state)
            return;

        boolean infect = SIRSAgent.INFECTION_PROBABILITY >= ThreadLocalRandom.current().nextDouble();

        if ( infect ) {
            this.durationInState = 0;
            this.state = SIRSState.Infected;
        }
    }

    private void handleInfectedAgent() {
        if ( this.durationInState >= SIRSAgent.DURATION_INFECTED ) {
            this.durationInState = 0;
            this.state = SIRSState.Recovered;
        } else {
            this.sendMessageToRandomNeighbour( MSG_CONTACT );
        }
    }

    private void handleRecoveredAgent() {
        if ( this.durationInState >= SIRSAgent.DURATION_IMMUNE ) {
            this.durationInState = 0;
            this.state = SIRSState.Susceptible;
        }
    }
}
