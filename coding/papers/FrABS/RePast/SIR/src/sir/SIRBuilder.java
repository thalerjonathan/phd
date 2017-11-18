package sir;

import repast.simphony.context.Context;
import repast.simphony.context.space.graph.NetworkBuilder;
import repast.simphony.dataLoader.ContextBuilder;

public class SIRBuilder implements ContextBuilder<Object> {

	public final static String CONTEXT_ID = "SIRStateChart";
	public final static String NETWORK_ID = "SIRStateChartNetwork";
	
	@Override
	public Context<Object> build(Context<Object> context) {
		context.setId(CONTEXT_ID);

		NetworkBuilder<Object> netBuilder = new NetworkBuilder<Object>(
				"connection network", context, true);
		netBuilder.buildNetwork();
		
		// TODO: instantiate agents
		//context.add(null);
		
		return context;
	}

}
