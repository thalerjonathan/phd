package Schelling;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;

/**
 * An extension of the Schelling segregation model from Growing
 * Artificial Societies by Epstein, Joshua M and Axtell, Robert. This
 * model implements rules from chapter VI.  One key difference is that
 * this model uses more than two agent types.
 * 
 * @author Eric Tatara 
 * @version 
 */

public class SchellingModel implements ContextBuilder<Object>{

	public Context<Object> build(Context<Object> context) {
		
		Parameters p = RunEnvironment.getInstance().getParameters();
		int numAgents = (Integer)p.getValue("initialNumAgents");
		int height = (Integer)p.getValue("worldHeight");
		int width = (Integer)p.getValue("worldWidth");
		
		// Create a new 2D grid on which the agents will move.  The inputs to the
		// GridFactory include the grid name, the context in which to place the grid,
		// and the grid parameters.  Grid parameters include the border specification,
		// random adder for populating the grid with agents, boolean for multiple occupancy,
		// and the dimensions of the grid. 
		//
		// The grid specified here has wrap around borders (torroidal world) and is
		// single occupancy.
		GridFactoryFinder.createGridFactory(null).createGrid("Grid", context, 
				new GridBuilderParameters<Object>(new WrapAroundBorders(), 
						new RandomGridAdder<Object>(), false, width, height));
		
	  // Create the initial agents and add to the context.
		for(int i=0; i<numAgents; i++){
			Agent agent = new Agent("Agent-"+i);
		
		  context.add(agent);
		}
		return context;
	}
}
