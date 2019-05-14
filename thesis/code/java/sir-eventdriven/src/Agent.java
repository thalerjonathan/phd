import java.util.List;
import java.util.Random;

public class Agent {
	
	private Integer aid;
	private int beta;
	private double gamma;
	private double delta;
	
	private SIRState state;

	private SIR sir;
	private Random r;
	
	public Agent(Integer aid, SIR sir, SIRState state, int beta, double gamma, double delta, Random r) {
		this.aid = aid;
		this.beta = beta;
		this.gamma = gamma;
		this.delta = delta;
		this.sir = sir;
		this.state = state;
		this.r = r;
		
		if (SIRState.SUSCEPTIBLE == this.state) {
			this.scheduleMakeContact();
			
		} else if (SIRState.INFECTED == this.state) {
			this.scheduleRecovery();
		}
	}
	
	public SIRState getState() {
		return this.state;
	}
	
	public void handleEvent(SIREvent event, List<Integer> ais) {
		if (SIRState.SUSCEPTIBLE == this.state) {
			this.susceptible(event, ais);
			
		} else if (SIRState.INFECTED == this.state) {
			this.infected(event);
			
		} else if (SIRState.RECOVERED == this.state) {
			// do nothing
		}
	}
	
	private void susceptible(SIREvent event, List<Integer> ais) {
		if (SIREventType.MAKE_CONTACT == event.type) {
			for (int i = 0; i < this.beta; i++) {
				int idx = r.nextInt(ais.size());
				Integer receiver = ais.get(idx);
				this.sir.scheduleEvent(SIREventType.CONTACT, 0, this.aid, receiver, SIRState.SUSCEPTIBLE);	
			}
			
			this.scheduleMakeContact();

		} else if (SIREventType.CONTACT == event.type) {
			if (event.data == SIRState.INFECTED) {
				if (r.nextDouble() <= this.gamma) {
					this.state = SIRState.INFECTED;
					this.scheduleRecovery();
				}
			}
		}
	}
	
	private void infected(SIREvent event) {
		if (SIREventType.CONTACT == event.type && SIRState.SUSCEPTIBLE == event.data) {
			this.sir.scheduleEvent(SIREventType.CONTACT, 0, this.aid, event.sender, SIRState.INFECTED);
			
		} else if (SIREventType.RECOVER == event.type) {
			this.state = SIRState.RECOVERED;
		}
	}
	
	private void scheduleMakeContact() {
		this.sir.scheduleEvent(SIREventType.MAKE_CONTACT, 1.0, this.aid, this.aid, null);
	}
	
	private void scheduleRecovery() {
		double illnessDuration = this.randomExp(1/this.delta);
		this.sir.scheduleEvent(SIREventType.RECOVER, illnessDuration, this.aid, this.aid, null);
	}
	
	private double randomExp(double lambda) {
		double v = r.nextDouble();
		return -(Math.log(v)) / lambda;
	}
}
