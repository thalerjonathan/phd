package sir;

import java.util.ArrayList;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.context.space.graph.NetworkBuilder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.graph.Network;

public class SIRBuilder implements ContextBuilder<ISIRAgent> {

	public final static String CONTEXT_ID = "SIR";
	public final static String NETWORK_ID = "sirnetwork";
	
	@Override
	public Context<ISIRAgent> build(Context<ISIRAgent> context) {
		context.setId(CONTEXT_ID);

		NetworkBuilder<ISIRAgent> netBuilder = new NetworkBuilder<ISIRAgent>(
				NETWORK_ID, context, true);
		Network<ISIRAgent> net = netBuilder.buildNetwork();
		
		Parameters params = RunEnvironment.getInstance().getParameters();
		boolean stateChartAgents = (Boolean) params.getValue("statechart_agents");
		int agentCount = (Integer) params.getValue("agents");
		int infectedCount = (Integer) params.getValue("infected_agents");
		double illnessDuration = (Double) params.getValue("illness_duration");
		int contacts = (Integer) params.getValue("contacts");
		double infectionProb = (Double) params.getValue("infection_probability");
		
		double contactRate = 1.0 / (double) contacts;
		
		List<ISIRAgent> agents = new ArrayList<ISIRAgent>();
		
		for (int i = 0; i < agentCount; ++i) {
			boolean infected = false;
			
			if ( i < infectedCount) {
				infected = true;
			}
			
			ISIRAgent a = null;
			
			if (stateChartAgents) 
				a = new SIRStateChartAgent(infected, contactRate, infectionProb, illnessDuration);
			else
				a = new SIRAgent();
			
			context.add( a );
			agents.add( a );
		}
		
		for (ISIRAgent from : agents) {
			for (ISIRAgent to : agents) {
				if (from != to) {
					net.addEdge(from, to);
				}
			}
		}
		
		return context;
	}
}
