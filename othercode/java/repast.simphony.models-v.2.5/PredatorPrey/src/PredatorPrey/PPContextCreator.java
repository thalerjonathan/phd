package PredatorPrey;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.RandomCartesianAdder;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.valueLayer.GridValueLayer;

/**
 * Predator prey model on a continuous space.
 * 
 * @author Eric Tatara
 * @author Nick Collier
 */
public class PPContextCreator implements ContextBuilder<SimpleAgent> {

	/**
	 * Builds and returns a context. Building a context consists of filling it with
	 * agents, adding projects and so forth. When this is called for the master context
	 * the system will pass in a created context based on information given in the
	 * model.score file. When called for subcontexts, each subcontext that was added
	 * when the master context was built will be passed in.
	 *
	 * @param context
	 * @return the built context.
	 */

	public Context build(Context<SimpleAgent> context) {
		int xdim = 50;   // The x dimension of the physical space
		int ydim = 50;   // The y dimension of the physical space

		// Create a new 2D grid to model the discrete patches of grass.  The inputs to the
		// GridFactory include the grid name, the context in which to place the grid,
		// and the grid parameters.  Grid parameters include the border specification,
		// random adder for populating the grid with agents, boolean for multiple occupancy,
		// and the dimensions of the grid.
		GridFactoryFinder.createGridFactory(null).createGrid("Simple Grid", context,
				new GridBuilderParameters<SimpleAgent>(new repast.simphony.space.grid.WrapAroundBorders(),
						new RandomGridAdder<SimpleAgent>(), true, xdim, ydim));

		// Create a new 2D continuous space to model the physical space on which the sheep
		// and wolves will move.  The inputs to the Space Factory include the space name, 
		// the context in which to place the space, border specification,
		// random adder for populating the grid with agents,
		// and the dimensions of the grid.
		ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null)
		.createContinuousSpace("Continuous Space", context, new RandomCartesianAdder<SimpleAgent>(),
				new repast.simphony.space.continuous.WrapAroundBorders(), xdim, ydim, 1);

		// Create a new 2D value layer to store the state of the grass grid.  This is
		// only used for visualization since it's faster to draw the value layer
		// in 2D displays compared with rendering each grass patch as an agent.
		GridValueLayer vl = new GridValueLayer("Grass Field", true, 
				new repast.simphony.space.grid.WrapAroundBorders(),xdim,ydim);
		
		context.addValueLayer(vl);
		
		// The environment parameters contain the user-editable values that appear in the GUI.
		//  Get the parameters p and then specifically the initial numbers of wolves and sheep.
		Parameters p = RunEnvironment.getInstance().getParameters();
		int numWolves = (Integer)p.getValue("initialnumberofwolves");
		int numSheep = (Integer)p.getValue("initialnumberofsheep");

		// Populate the root context with the initial agents
		// Iterate over the number of wolves
		for (int i = 0; i < numWolves; i++) {
			Wolf wolf = new Wolf();             // create a new wolf
			context.add(wolf);                  // add the new wolf to the root context
		}
		// Iterate over the number of sheep
		for (int i = 0; i < numSheep; i++) {
			Sheep sheep = new Sheep();          // create a new sheep
			context.add(sheep);                 // add a new sheep to the root context
		}

		// Populate the patch grid with grass
		// Iterate over the dimensions of the patch grid
		for (int i=0; i<xdim; i++){
			for (int j=0; j<ydim; j++){
				Grass grass = new Grass(context,i,j);				// create a new grass
			}
		}

	  // If running in batch mode, tell the scheduler when to end each run.
		if (RunEnvironment.getInstance().isBatch()){
			
			double endAt = (Double)p.getValue("runlength");     
			RunEnvironment.getInstance().endAt(endAt);
		}
		return context;                       
	}
}