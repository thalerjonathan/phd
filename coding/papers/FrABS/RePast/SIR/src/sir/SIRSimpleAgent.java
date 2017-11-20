package sir;

import cern.jet.random.Exponential;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;

public class SIRSimpleAgent implements ISIRAgent {

	private enum SirState {
		SUSCEPTIBLE, INFECTED, RECOVERED
	}
	
	private SirState state;
	
	private double infectionProb;
	private double illnessDuration;
	
	private Exponential contactExp;
	private Exponential illnessExp;
	
	private final static double SAMPLING_DELTA = 1.0;
	
	public SIRSimpleAgent(boolean initiallyInfected, double contactRate, double infectionProb, double illnessDuration) {
		this.infectionProb = infectionProb;
			
		this.contactExp = RandomHelper.createExponential(SAMPLING_DELTA/contactRate);
		this.illnessExp = RandomHelper.createExponential(SAMPLING_DELTA/illnessDuration);
		
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
		int randomContacts = this.contactExp.nextInt();
		
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
		this.illnessDuration -= SAMPLING_DELTA;
		if (this.illnessDuration <= 0) {
			this.state = SirState.RECOVERED;
		}
	}
	
	private void infect() {
		this.state = SirState.INFECTED;
		this.illnessDuration = this.illnessExp.nextDouble();
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
