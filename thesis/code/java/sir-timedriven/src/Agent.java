import java.util.List;
import java.util.Random;

public class Agent {
	
	private double beta;
	private double gamma;
	private double delta;
	
	private SIRState state;

	private Random r;
	
	public Agent(Random r, SIRState state, double beta, double gamma, double delta) {
		this.beta = beta;
		this.gamma = gamma;
		this.delta = delta;
		this.r = r;
		this.state = state;
	}
	
	public SIRState getState() {
		return this.state;
	}
	
	public void step(double dt, List<SIRState> neighbours) {
		if (SIRState.SUSCEPTIBLE == this.state) {
			this.susceptible(dt, neighbours);
			
		} else if (SIRState.INFECTED == this.state) {
			this.infected(dt);
			
		} else if (SIRState.RECOVERED == this.state) {
			// do nothing
		}
	}
	
	private void susceptible(double dt, List<SIRState> neighbours) {
		if (this.occasionally(1 / this.beta, dt)) {
			int idx = (int) (r.nextDouble() * neighbours.size());
			SIRState s = neighbours.get(idx);
			if (s == SIRState.INFECTED) {
				if (r.nextDouble() <= this.gamma) {
					this.state = SIRState.INFECTED;
				}
			}
		}
	}
	
	private void infected(double dt) {
		if (this.occasionally(this.delta, dt)) {
			this.state = SIRState.RECOVERED;
		}
	}
	
	private boolean occasionally(double lamba, double dt) {
		double p = 1 - Math.exp(-(dt/lamba));
		return (r.nextDouble() <= p);
	}
}
