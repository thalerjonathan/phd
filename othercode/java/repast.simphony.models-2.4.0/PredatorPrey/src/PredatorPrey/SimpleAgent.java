package PredatorPrey;

import repast.simphony.annotate.AgentAnnot;
import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.grid.Grid;
import repast.simphony.util.ContextUtils;

/**
 * SimpleAgent is the parent class of all other agents.
 * 
 * @author Eric Tatara 
 */

@AgentAnnot(displayName = "Agent")
public class SimpleAgent {
	private double energy;       // The energy level of the agent
	private double heading;      // The heading in degrees of the agent
	
	// Shedule the step method for agents.  The method is scheduled starting at 
	// tick one with an interval of 1 tick.  Specifically, the step starts at 1, and
	// and recurs at 2,3,4,...etc
	@ScheduledMethod(start = 1, interval = 1, shuffle=true)
	public void step() {
		// Override by subclasses
	}
	
  // Move the agent
	public void move() {
		// The agent is aware of its location in the continuous space and
		// which grass patch it is on
		
		// Get the context in which the agent is residing
		Context context = ContextUtils.getContext(this);
		
		// Get the patch grid from the context
		Grid patch = (Grid) context.getProjection("Simple Grid");
		
		// Get the continuous space from the context
		ContinuousSpace space = (ContinuousSpace) context.getProjection("Continuous Space");
		
		NdPoint point = space.getLocation(this);  // Get the agent's point coordinate from the space

		double x = point.getX();			// The x coordinate on the 2D continuous space
		double y = point.getY();      // The y coordinate on the 2D continuous space
		
		// Randomly change the current heading plus or minus 50 degrees
		double sgn = Math.random() - 0.5;       // a value between -0.5 and 0.5
		if (sgn > 0)
		  heading = heading + Math.random()*50;
		else
			heading = heading - Math.random()*50;
		
		// Move the agent on the space by one unit according to its new heading
		space.moveByVector(this, 1, Math.toRadians(heading),0,0);
		
		// Move the agent to its new patch (note the patch may not actually change)
		patch.moveTo(this, (int)x, (int)y);	
	}
	
  // Kill the agent
	public void die(){
    // Get the context in which the agent resides.
		Context context = ContextUtils.getContext(this);
		
    // Remove the agent from the context if the context is not empty
		if (context.size() > 1)
		  context.remove(this); 
    // Otherwise if the context is empty, end the simulation
		else
			RunEnvironment.getInstance().endRun();
	}
	
	public int isSheep() {
		return 0;
	}
	
	public int isWolf() {
		return 0;
	}

	public double getEnergy() {
		return energy;
	}

	public void setEnergy(double energy) {
		this.energy = energy;
	}

	public double getHeading() {
		return heading;
	}

	public void setHeading(double heading) {
		this.heading = heading;
	}
	
}
