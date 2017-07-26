package PredatorPrey;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

/**
 * The wolf agent.
 * 
 * @author Eric Tatara 
 */

public class Wolf extends SimpleAgent {

	// This constructor is used to create an offspring
	public Wolf (double energy){
		this.setEnergy(energy);               // assign the offspring energy
		this.setHeading(Math.random()*360);   // randomize the heading from 0-360 degrees
	}

	// This constructor is used to create initial wolves from the context creator
	public Wolf(){
		// Get the value of the wolf gain from food from the environment parameters
		Parameters p = RunEnvironment.getInstance().getParameters();
		double gain = (Double)p.getValue("wolfgainfromfood");

		this.setEnergy(Math.random() * 2 * gain);    // set the initial energy
		this.setHeading(Math.random()*360);          //  and initial heading
	}
	
  @Override
	public void step() {
    // Get the context in which the wolf resides.
		Context context = ContextUtils.getContext(this);

		// Move the wolf
		move();

		// Reduce the wolf's energy by one unit
		this.setEnergy(this.getEnergy() - 1);

		// Catch sheep
    // Get the patch grid from the context
		Grid patch = (Grid) context.getProjection("Simple Grid");
		
		// Get the wolf's current patch
		GridPoint point = patch.getLocation(this);
		
		int x = point.getX();    // The x-ccordinate of the wolf's current patch
		int y = point.getY();    // The y-ccordinate of the wolf's current patch

		// Get the wolf gain from food from the user parameters
		Parameters p = RunEnvironment.getInstance().getParameters();
		double gain = (Double)p.getValue("wolfgainfromfood");

		// Find sheep at the patch and eat it if one exists
		Sheep sheep = null;
		for (Object o: patch.getObjectsAt(x,y)){
			if (o instanceof Sheep)
				sheep = (Sheep)o;
		}
		// If there is a sheep on the patch then eat it
		if (sheep != null){
			sheep.die();              // kill the sheep
			this.setEnergy(this.getEnergy() + gain);   // increment the wolf's energy
		}

		// Reproduce the wolf 
    // Get the reproduction rate from the user parameters
		double rate = (Double)p.getValue("wolfreproduce");

    //	Spawn a new wolf if a random draw on [0,100) < reproduction rate
		if (100*Math.random() < rate){
			this.setEnergy(this.getEnergy() / 2);      // divide the parent's energy in half
			Wolf wolf = new Wolf(this.getEnergy());    // create a new wolf offspring and assing its energy
			context.add(wolf);	                       // add the offspring to the root context
		}

		// Death
		if (this.getEnergy() < 0)
			die();
	}

	
  // Public getter for the data gatherer for counting 
	@Override
	public int isWolf() {
		return 1;
	}
}