import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;

public class SIR {
	private PriorityQueue<SIREvent> events;
	private List<Agent> agents;
	
	private double t;
	
	public SIR(int susceptible, int infected, int recovered, int beta, double gamma, double delta) {
		int agentCount = susceptible + infected + recovered;
		
		this.agents = new ArrayList<>(agentCount);
		this.events = new PriorityQueue<>(100, new Comparator<SIREvent>() {
			@Override
			public int compare(SIREvent arg0, SIREvent arg1) {
				if (arg0.timeStamp < arg1.timeStamp) {
					return -1;
				} else if (arg0.timeStamp >= arg1.timeStamp) {
					return 1;
				}
				
				return 0;
			}
			
		});
		
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
	
	public void scheduleEvent(SIREventType type, double dt, Agent sender, Agent receiver) {
		SIREvent sirEvent = new SIREvent();
		sirEvent.timeStamp = this.t + dt;
		sirEvent.type = type;
		sirEvent.sender = sender;
		sirEvent.receiver = receiver;
		
		this.events.add(sirEvent);
	}
	
	public List<SIRStep> run(double tMax)  {
		this.t = 0;
		List<SIRStep> steps = new ArrayList<>();

		while (true) {
			System.out.println("events = " + this.events);
			
			SIREvent evt = this.events.poll();
			if (null == evt) {
				break;
			}
			
			this.t = evt.timeStamp;
			
			evt.receiver.handleEvent(evt);
		
			System.out.println("t = " + this.t + ", events = " + this.events);
			
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
