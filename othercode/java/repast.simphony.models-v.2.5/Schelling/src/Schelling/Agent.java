package Schelling;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.query.space.grid.VNQuery;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.util.ContextUtils;

/**
 * The agent for the Schelling model. 
 * 
 * @author Eric Tatara 
 */

public class Agent {

	private int maxAge, currentAge, type;
	private double percentLikeNeighbors;
	private String id;
	private double numLikeMe;
	private double neighborCount;
  private int numberOfAgentTypes = 3;
	
	public Agent(String id) {

		this.id = id;

		Parameters p = RunEnvironment.getInstance().getParameters();

		int minDeathAge = (Integer)p.getValue("minDeathAge");
		int maxDeathAge = (Integer)p.getValue("maxDeathAge");

		// The agent is randomly a type from 1 to numberOfAgentTypes-1
		this.type = RandomHelper.nextIntFromTo(0, numberOfAgentTypes-1);

		// The max age of the agent is uniformly distributed on [minDeathAge, maxDeathAge]
		this.maxAge = RandomHelper.nextIntFromTo(minDeathAge, maxDeathAge);

		// The percentage of neighbors like this agent that satisfies its preferences
		this.percentLikeNeighbors = (Double)p.getValue("percentLikeNeighbors");
	}

  //Schedule the step method for agents.  The method is scheduled starting at 
	// tick one with an interval of 1 tick.  Specifically, the step starts at 0, and
	// and recurs at 1,2,3,...etc
	@ScheduledMethod(start=0,interval=1)
	public void step() {
	  // Get the context in which the agent is residing
		Context<Agent> context = (Context)ContextUtils.getContext(this);
		
	  // Get the grid from the context
		Grid<Agent> grid = (Grid)context.getProjection("Grid");

		// Initially the agent will look for a new site if is unhappy
		// with its current location and will continue to look for
		// a new site until a satisfactory site is found.
		boolean lookingForNewSite = true;

		while(lookingForNewSite){  // loop until neighbor conditions satisfied

			// Query Von Neuman neighbors in grid
			VNQuery<Agent> query = new VNQuery<Agent>(grid, this);

			numLikeMe = 0;      // number of neighbors like me
			neighborCount = 0;  // number of neighbors

			// Check VN neighbors and sum like types
			for (Agent agent : query.query()){
				if (agent.getType() == this.getType())
					numLikeMe++;

				neighborCount++;
			}

			// If percent satisfied, don't move
			if (numLikeMe / neighborCount >= percentLikeNeighbors){
				lookingForNewSite = false;
			}
			else{
				int width  = grid.getDimensions().getWidth();
				int height = grid.getDimensions().getHeight();

				// otherwise move to another spot
				int x = RandomHelper.nextIntFromTo(1, width-1);
				int y = RandomHelper.nextIntFromTo(1, height-1);

				while(grid.getObjectAt(x,y) != null){
					x = RandomHelper.nextIntFromTo(1, width-1);
					y = RandomHelper.nextIntFromTo(1, height-1);
				}

				// Move to new location
				grid.moveTo(this, x,y);
			}
		}

		// increase my age
		currentAge++;

		// die if age is greater than my max age
		if (currentAge >= maxAge) 
			this.die();
	}


	//Kill the agent
	public void die(){
		// Get the context in which the agent resides.
		Context<Agent> context = (Context)ContextUtils.getContext(this);

		// Remove the agent from the context 
		context.remove(this);   

		// Spawn a new agent 
		Agent child = new Agent(this.id);

		// Add the new agent to the context
		context.add(child);
	}

	// Generic Getters for probing, logging, etc
	public int getMaxAge() {
		return maxAge;
	}
	public int getCurrentAge() {
		return currentAge;
	}
	public int getType() {
		return type;
	}
	public String getId() {
		return id;
	}
	public String toString(){
		return this.id;
	}
	public double getNumLikeMe() {
		return numLikeMe;
	}
	public double getNeighborCount() {
		return neighborCount;
	}
}
