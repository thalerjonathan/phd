import java.util.List;

public class Agent {
	
	private int beta;
	private double gamma;
	private double delta;
	
	private SIRState state;

	private SIR sir;
	private List<Agent> agents;
	
	public Agent(SIR sir, List<Agent> agents, SIRState state, int beta, double gamma, double delta) {
		this.agents = agents;
		this.beta = beta;
		this.gamma = gamma;
		this.delta = delta;
		this.sir = sir;
		this.state = state;
		
		if (SIRState.SUSCEPTIBLE == this.state) {
			this.scheduleMakeContact();
			
		} else if (SIRState.INFECTED == this.state) {
			this.scheduleRecovery();
		}
	}
	
	public SIRState getState() {
		return this.state;
	}
	
	public void handleEvent(SIREvent event) {
		if (SIRState.SUSCEPTIBLE == this.state) {
			this.susceptible(event);
			
		} else if (SIRState.INFECTED == this.state) {
			this.infected(event);
			
		} else if (SIRState.RECOVERED == this.state) {
			// do nothing
		}
	}
	
	private void susceptible(SIREvent event) {
		if (SIREventType.MAKE_CONTACT == event.type) {
			for (int i = 0; i < this.beta; i++) {
				int idx = (int) (Math.random() * this.agents.size());
				Agent a = this.agents.get(idx);
				
				if (a == this) {
					continue;
				}
				
				this.sir.scheduleEvent(SIREventType.CONTACT, 0, this, a);	
			}
			
			this.scheduleMakeContact();

		} else if (SIREventType.CONTACT == event.type) {
			if (event.sender == this)
				return;
			
			if (event.sender.getState() == SIRState.INFECTED) {
				if (Math.random() <= this.gamma) {
					this.state = SIRState.INFECTED;
					this.scheduleRecovery();
				}
			}
		}
	}
	
	private void infected(SIREvent event) {
		if (SIREventType.CONTACT == event.type) {
			if (event.sender == this)
				return;
			
			this.sir.scheduleEvent(SIREventType.CONTACT, 0, this, event.sender);
			
		} else if (SIREventType.RECOVER == event.type) {
			this.state = SIRState.RECOVERED;
		}
	}
	
	private void scheduleMakeContact() {
		this.sir.scheduleEvent(SIREventType.MAKE_CONTACT, 1.0, this, this);
	}
	
	private void scheduleRecovery() {
		double illnessDuration = this.randomExp(1/delta);
		this.sir.scheduleEvent(SIREventType.RECOVER, illnessDuration, this, this);
	}
	
	private double randomExp(double lambda) {
		double r = Math.random();
		return -(Math.log(r)) / lambda;
	}
}
