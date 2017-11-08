package SugarScape;

import repast.simphony.context.Context;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;

/**
 * A partial implementation of the Sugar Scape simulation from Growing
 * Artificial Societies by Epstein, Joshua M and Axtell, Robert. This
 * model implements rules G, R, and M from chapter II.
 * 
 * The source code is heavily annotated as example of a simulation built using
 * the Repast toolkit. See the html API documentation for the details of the
 * framework objects.
 * 
 * @author Eric Tatara
 * @author Nick Collier  
 * @version 
 */

public class SugarScape implements ContextBuilder<Object>{

	public Context<Object> build(Context<Object> context) {
		
	  // The sugarFile contains the initial/max sugar values for every point
		// on the 2D sugarspace.
		String sugarFile = "misc/sugarspace.pgm";
		
		Parameters p = RunEnvironment.getInstance().getParameters();
		int numAgents = (Integer)p.getValue("initialNumAgents");
		
		SugarSpace sugarSpace = new SugarSpace(sugarFile);
		context.addSubContext(sugarSpace);
		context.add(sugarSpace);
		
		// Create the initial agents and add to the sugar space.
		for(int i=0; i<numAgents; i++){
			SugarAgent agent = new SugarAgent();
		
		  sugarSpace.add(agent);
		}
		
		// If running in batch mode, schedule the sim stop time
		double endTime = 500.0;
		
		if(RunEnvironment.getInstance().isBatch())
			RunEnvironment.getInstance().endAt(endTime);
		
		return context;
	}
}
