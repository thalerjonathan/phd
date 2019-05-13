import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

public class SIR {
	private PriorityQueue<SIREvent> events;
	private Map<Integer, Agent> agents;
	
	private double t;
	
	public SIR(int susceptible, int infected, int recovered, int beta, double gamma, double delta) {
		int agentCount = susceptible + infected + recovered;
		
		this.agents = new HashMap<>(agentCount);
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
		
		int idx = 0;
		
		for (int i = 0; i < susceptible; i++) {
			this.agents.put(idx, new Agent(idx, this, SIRState.SUSCEPTIBLE, beta, gamma, delta));
			idx++;
		}
		
		for (int i = 0; i < infected; i++) {
			this.agents.put(idx, new Agent(idx, this, SIRState.INFECTED, beta, gamma, delta));
			idx++;
		}
		
		for (int i = 0; i < recovered; i++) {
			this.agents.put(idx, new Agent(idx, this, SIRState.RECOVERED, beta, gamma, delta));
			idx++;
		}
	}
	
	public void scheduleEvent(SIREventType type, double dt, Integer sender, Integer receiver, Object data) {
		SIREvent sirEvent = new SIREvent();
		sirEvent.timeStamp = this.t + dt;
		sirEvent.type = type;
		sirEvent.sender = sender;
		sirEvent.receiver = receiver;
		sirEvent.data = data;
		
		this.events.add(sirEvent);
	}
	
	public List<SIRStep> run(double tMax)  {
		this.t = 0;
		List<SIRStep> steps = new ArrayList<>();
		List<Integer> ais   = new ArrayList<>(this.agents.keySet());
		
		steps.add(this.aggregate());
		
		while (true) {
			SIREvent evt = this.events.poll();
			if (null == evt) {
				break;
			}

			this.t = evt.timeStamp;
			
			Agent receiver = this.agents.get(evt.receiver);
			receiver.handleEvent(evt, ais);
		
			SIRStep step = this.aggregate();
			
			if (steps.get(steps.size() - 1).t == step.t) {
				steps.remove(steps.size() - 1);
			}
			
			steps.add(step);
			
			if (step.i == 0 && tMax == 0) {
				break;
			} else {
				if (t >= tMax) {
					break;
				}
			}
		}
		
		return steps;
	}
	
	private SIRStep aggregate() {
		int s = 0;
		int i = 0;
		int r = 0;

		for (Agent a : agents.values()) {
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
		
		return step;
	}
}
