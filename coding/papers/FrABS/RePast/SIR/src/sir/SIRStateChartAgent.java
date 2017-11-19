package sir;

import chart.SIRStateChart;
import repast.simphony.context.Context;
import repast.simphony.space.graph.Network;
import repast.simphony.ui.probe.ProbedProperty;
import repast.simphony.util.ContextUtils;

public class SIRStateChartAgent implements ISIRAgent {
	
	private double contactRate;
	private double infectionProb;
	private double illnessDuration;
	
	public final static String CONTACT_INFECTED = "CONTACT_INFECTED";
	
	private final static String STATE_SUSCEPTIBLE = "Susceptible";
	private final static String STATE_INFECTED = "Infected";
	private final static String STATE_RECEOVERED = "Recovered";
	
	@ProbedProperty(displayName="PersonHallStatechart")
	private SIRStateChart state = SIRStateChart.createStateChart(this, 0);
	
	public SIRStateChartAgent(boolean infected, double contactRate, double infectionProb, double illnessDuration) {
		if (infected) {
			this.state.activateState(STATE_INFECTED);
		}
		
		this.contactRate = contactRate;
		this.infectionProb = infectionProb;
		this.illnessDuration = illnessDuration;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void makeContact() {
		Context<ISIRAgent> context = (Context<ISIRAgent>) ContextUtils.getContext(this);
		Network<ISIRAgent> net = (Network<ISIRAgent>) context.getProjection( SIRBuilder.NETWORK_ID );

		ISIRAgent a = net.getRandomAdjacent(this);
		/*
		boolean infected = a.isInfected();
		if (infected) {
			this.state.receiveMessage(CONTACT_INFECTED);
		}
		*/
		a.inContactWith(this);
	}
	
	@Override
	public void infect() {
		this.state.receiveMessage(CONTACT_INFECTED);
	}
	
	@Override
	public void inContactWith(ISIRAgent a) {
		if (this.isInfected()) {
			a.infect();
		}
	}
	
	@Override
	public double getInfectionProb() {
		return this.infectionProb;
	}
	
	@Override
	public double getContactRate() {
		return this.contactRate;
	}
	
	@Override
	public double getIllnessDuration() {
		return this.illnessDuration;
	}

	@Override
	public boolean isSusceptible() {
		return this.state.withinState( STATE_SUSCEPTIBLE );
	}

	@Override
	public boolean isInfected() {
		return this.state.withinState( STATE_INFECTED );
	}

	@Override
	public boolean isRecovered() {
		return this.state.withinState( STATE_RECEOVERED );
	}
}
