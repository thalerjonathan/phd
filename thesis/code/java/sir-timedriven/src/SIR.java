import java.util.ArrayList;
import java.util.List;

public class SIR {
	private List<Agent> agents;
	
	public SIR(int susceptible, int infected, int recovered, double beta, double gamma, double delta) {
		int agentCount = susceptible + infected + recovered;
		
		this.agents = new ArrayList<>(agentCount);
		
		for (int i = 0; i < susceptible; i++) {
			this.agents.add(new Agent(this, this.agents, SIRState.SUSCEPTIBLE, beta, gamma, delta));
		}
		
		for (int i = 0; i < infected; i++) {
			this.agents.add(new Agent(this, this.agents, SIRState.INFECTED, beta, gamma, delta));
		}
		
		for (int i = 0; i < recovered; i++) {
			this.agents.add(new Agent(this, this.agents, SIRState.RECOVERED, beta, gamma, delta));
		}
	}
	
	public List<SIRStep> run(double tMax, double dt)  {
		double t = 0;
		List<SIRStep> steps = new ArrayList<>();
		
		while (true) {
			for (Agent a : this.agents) {
				a.step(dt);
			}
			
			t = t + dt;
			
			int s = 0;
			int i = 0;
			int r = 0;
			for (Agent a : this.agents ) {
				if (SIRState.SUSCEPTIBLE == a.getState() ) {
					s++;
				} else if (SIRState.INFECTED == a.getState() ) {
					i++;
				} else if (SIRState.RECOVERED == a.getState() ) {
					r++;
				}
			}
			
			SIRStep step = new SIRStep();
			step.t = t;
			step.s = s;
			step.i = i;
			step.r = r;
			
			steps.add(step);
			
			if (i == 0 && tMax == 0) {
				break;
			} else {
				if (t >= tMax) {
					break;
				}
			}
		}
		
		return steps;
	}
}
