package sir;

import chart.SIRStateChart;
import repast.simphony.ui.probe.ProbedProperty;

public class SIRStateChartAgent implements ISIRAgent {
	
	private boolean initiallyInfected;
	
	private double contactRate;
	private double infectionProb;
	private double illnessDuration;
	
	public final static String CONTACT_INFECTED = "CONTACT_INFECTED";
	
	private final static String STATE_SUSCEPTIBLE = "Susceptible";
	private final static String STATE_INFECTED = "Infected";
	private final static String STATE_RECEOVERED = "Recovered";
	
	@ProbedProperty(displayName="SIR Statechart")
	private SIRStateChart state = SIRStateChart.createStateChart(this, 0);
	
	public SIRStateChartAgent(boolean initiallyInfected, double contactRate, double infectionProb, double illnessDuration) {
		this.contactRate = contactRate;
		this.infectionProb = infectionProb;
		this.illnessDuration = illnessDuration;
		
		this.initiallyInfected = initiallyInfected;
		
		if (initiallyInfected) {
			this.state.activateState(STATE_INFECTED);
		}	
	}
	
	public boolean isInitiallyInfected() {
		return this.initiallyInfected;
	}
	
	@Override
	public void makeContact() {
		ISIRAgent a = SIRBuilder.getRandomNeighbour(this);
		if (a.isInfected())
			this.state.receiveMessage(CONTACT_INFECTED);
	}
	
	public double getInfectionProb() {
		return this.infectionProb;
	}
	
	public double getContactRate() {
		return this.contactRate;
	}
	
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
