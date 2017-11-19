package sir;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;

public class SIRSimpleAgent implements ISIRAgent {

	private enum SirState {
		SUSCEPTIBLE, INFECTED, RECOVERED
	}
	
	private SirState state;
	
	private double contactRate;
	private double infectionProb;
	private double illnessDuration;
	
	private double illnessEnd;
	
	private final static double SAMPLING_DELTA = 1.0;
	
	public SIRSimpleAgent(boolean initiallyInfected, double contactRate, double infectionProb, double illnessDuration) {
		this.contactRate = contactRate;
		this.infectionProb = infectionProb;
		this.illnessDuration = illnessDuration;
	
		this.illnessEnd = 0;
		
		this.state = SirState.SUSCEPTIBLE;
		if ( initiallyInfected ) {
			infect();
		}
	}
	
	@ScheduledMethod(start = 0, interval = SAMPLING_DELTA)
	public void action() {
		if (this.isSusceptible()) {
			this.makeContact();
		} else if (this.isInfected()) {
			this.infected();
		}
	}
	
	@Override
	public void makeContact() {
		// TODO: draw from random-distribution, must be based upon SAMPLING_DELTA
		int randomContacts = (int) contactRate;
		
		for (int i = 0; i < randomContacts; ++i) {
			ISIRAgent a = SIRBuilder.getRandomAgent(this);
			if (a.isInfected()) {
				double r = Math.random();
				if (r <= this.infectionProb) {
					infect();
				}
			}
		}
	}

	private void infected() {
		this.illnessEnd -= SAMPLING_DELTA;
		if (this.illnessEnd <= 0) {
			this.state = SirState.RECOVERED;
		}
	}
	
	private void infect() {
		RandomHelper.createExponential(1/this.illnessDuration);
		this.state = SirState.INFECTED;
		this.illnessEnd = RandomHelper.getExponential().nextDouble();
	}
	
	@Override
	public boolean isSusceptible() {
		return SirState.SUSCEPTIBLE == this.state;
	}

	@Override
	public boolean isInfected() {
		return SirState.INFECTED == this.state;
	}

	@Override
	public boolean isRecovered() {
		return SirState.RECOVERED == this.state;
	}
}
