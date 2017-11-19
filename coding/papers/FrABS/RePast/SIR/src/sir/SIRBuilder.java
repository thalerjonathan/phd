package sir;

import java.util.ArrayList;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.util.ContextUtils;

public class SIRBuilder implements ContextBuilder<ISIRAgent> {

	public final static String CONTEXT_ID = "SIR";
	
	@Override
	public Context<ISIRAgent> build(Context<ISIRAgent> context) {
		context.setId(CONTEXT_ID);
		
		Parameters params = RunEnvironment.getInstance().getParameters();
		boolean stateChartAgents = (Boolean) params.getValue("statechart_agents");
		int agentCount = (Integer) params.getValue("agents");
		int infectedCount = (Integer) params.getValue("infected_agents");
		double illnessDuration = (Double) params.getValue("illness_duration");
		int contacts = (Integer) params.getValue("contacts");
		double infectionProb = (Double) params.getValue("infection_probability");
	
		List<ISIRAgent> agents = new ArrayList<ISIRAgent>();
		
		for (int i = 0; i < agentCount; ++i) {
			boolean infected = false;
			ISIRAgent a = null;
			
			if (i < infectedCount)
				infected = true;
		
			if (stateChartAgents) 
				a = new SIRStateChartAgent(infected, contacts, infectionProb, illnessDuration);
			else
				a = new SIRAgent();
			
			context.add( a );
			agents.add( a );
		}

		return context;
	}
	
	@SuppressWarnings("unchecked")
	public static ISIRAgent getRandomAgent(ISIRAgent self) {
		Context<ISIRAgent> context = (Context<ISIRAgent>) ContextUtils.getContext(self);
		
		while(true) {
			ISIRAgent a = context.getRandomObject();
			if (a != self)
				return a;
		}
	}
}
