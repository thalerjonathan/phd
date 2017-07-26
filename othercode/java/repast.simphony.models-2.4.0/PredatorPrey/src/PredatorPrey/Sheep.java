package PredatorPrey;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

/**
 * The Sheep agent.
 * 
 * @author Eric Tatara 
 */
public class Sheep extends SimpleAgent {
	
  // This constructor is used to create an offspring
	public Sheep(double energy){
		this.setEnergy(energy);               // assign the offspring energy
		this.setHeading(Math.random()*360);   // randomize the heading from 0-360 degrees
	}
  
	// This constructor is used to create initial wolves from the context creator
	public Sheep(){
  //	 Get the value of the sheeep gain from food from the environment parameters
		Parameters p = RunEnvironment.getInstance().getParameters();
		double gain = (Double)p.getValue("sheepgainfromfood");
		
		this.setEnergy(Math.random() * 2 * gain);    // set the initial energy
		this.setHeading(Math.random()*360);          //  and initial heading
	}
	
	@Override
	public void step() {
    //	Get the context in which the sheep resides.
		Context context = ContextUtils.getContext(this);
		
		// Move the sheep
		move();
		
    //	Reduce the sheep's energy by one unit
		this.setEnergy(this.getEnergy() - 1);
		
		// Eat Grass
		// Get the patch grid from the context
		Grid patch = (Grid) context.getProjection("Simple Grid");
		
		// Get the sheep's current patch
		GridPoint point = patch.getLocation(this);
		
		int x = point.getX();   // The x-ccordinate of the sheep's current patch
		int y = point.getY();   // The y-ccordinate of the sheep's current patch
		
		// Get the sheep gain from food from the user parameters
		Parameters p = RunEnvironment.getInstance().getParameters();
		double gain = (Double)p.getValue("sheepgainfromfood");

		// Find the grass at the patch and eat it if it is alive
		Grass grass = null;                   
		for (Object o : patch.getObjectsAt(x,y)){
			if (o instanceof Grass)
				grass = (Grass)o;
		}
		// If there is a grass and it is alive, then eat it
		if (grass != null && grass.isAlive()){
			grass.consume();                    // eat the grass
			this.setEnergy(this.getEnergy() + gain);  // increment the sheep's energy
		}
		
		// Reproduce the sheep
		// Get the reproduction rate from the user parameters
		double rate = (Double)p.getValue("sheepreproduce");
		
    // Spawn a new sheep if a random draw on [0,100) < reproduction rate
		if (100*Math.random() < rate){
			this.setEnergy(this.getEnergy() / 2);	       // divide the parent's energy in half
			Sheep sheep = new Sheep(this.getEnergy());   // create a new sheep offspring and assing its energy
			context.add(sheep);                          // add the offspring to the root context
		}

		// Kill the sheep if its energy drops below zero
		if (this.getEnergy() < 0)
		 die();
	}

	// Public getter for the data gatherer for counting sheep
	@Override
	public int isSheep() {
		return 1;
	}
}