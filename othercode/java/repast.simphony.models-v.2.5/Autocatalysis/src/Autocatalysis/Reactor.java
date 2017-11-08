package Autocatalysis;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.query.space.grid.VNQuery;
import repast.simphony.space.grid.Grid;
import repast.simphony.util.ContextUtils;
import repast.simphony.valueLayer.GridValueLayer;

/**
 * Each reactor is connected to its neighbors in a 2D von Neuman neighborhood.  
 * The cubic autocatalytic reaction for a single species is
 *           
 *    R + 2P -> 3P
 *         
 *         P -> D
 *  
 * where R is the resource, P is the species, and D is a dead (inert species),
 * with corresponding growth and death rates.  See
 *   
 *  Tatara, E., I. Birol, F. Teymour, and A. Cinar, Static and Dynamic Behavior
 *   of Autocatalytic Replicators in Reactor Networks, Industrial and Engineering
 *   Chemistry Research, 43, 3972-3993, 2004.
 *   
 * for a full description of the model.  The dimensionless ODEs for R and P are:
 *   
 *    dr
 *    --  = -krp^2 + f(1-r) + g(r_up + r_down + r_left + r_right - 4r)
 *    dt
 *  
 *    dp
 *    --  = krp^2 - p(f+d) + g(p_up + p_down + p_left + p_right - 4p) 
 *    dt
 *  
 * where k is the growth rate, d is the death rate, f is the flow rate, and
 * g is the interconnection flow rate between neighbors. 
 *  
 * The ODEs are solved numerically using forward Euler integration, which 
 * provides a simple and efficient solution, although is not necessarily 
 * physically realistic considering that the model is highly nonlinear.
 * 
 * Note that the model behavior is highly sensitive to the parameters.  
 * Experiment with different values for the flow rates and kinetics, but start 
 * off with small differences from the default values.  The integration time
 * step may also need to be changed for some regimes.
 *  
 * @author Eric Tatara
 *
 */
public class Reactor {

	public double resource; // the resource concentration
	public double species;  // the species concentration
	
	public static final double delta = 0.5; // integration time step
	
	public Reactor(){
		resource = 1.0;   // initially 1 for all reactors
		species  = 0.0;   // initially 0 for all reactors
	}
	
	/**
	 * The step method updates the reactor states for the given interval
	 */
	@ScheduledMethod(start=0, interval=Reactor.delta, shuffle=true)
	public void step(){
	  // Get the context in which the agent is residing
    Context<Reactor> context = (Context)ContextUtils.getContext(this);
		
	  // Get the grid from the context
		Grid<Reactor> grid = (Grid)context.getProjection("Grid");

	  // Query Von Neuman neighbors in grid
		VNQuery<Reactor> query = new VNQuery<Reactor>(grid, this);
		
		// Sum up the concentrations in the neighboring reactors
		double neighborResource = 0;
		double neighborSpecies = 0;
		
		for (Reactor cell : query.query()){
			neighborResource += cell.getResource();
			neighborSpecies += cell.getSpecies();
		}
		
		Parameters parm = RunEnvironment.getInstance().getParameters();
		double flow = (Double)parm.getValue("flowRate");
		double interaction = (Double)parm.getValue("interactionFlowRate");
		double growthRate = (Double)parm.getValue("growthRate");
		double deathRate = (Double)parm.getValue("deathRate");
		
		// Solve for resource concentration
		double r = -growthRate * resource * Math.pow(species,2) 
		              + flow * (1 - resource) 
		              + interaction * (neighborResource - 4 * resource);
		
		resource = r * delta + resource;
		
		// solve for species concentration
		double p =  growthRate * resource * Math.pow(species,2) 
                  - species * (deathRate + flow) 
                  + interaction * (neighborSpecies - 4 * species);
		
		
		species  = p * delta + species;
		
		// Store the values of the concentrations in value layers for better animation speed
		GridValueLayer resourceLayer = (GridValueLayer)context.getValueLayer("Resource");
		GridValueLayer speciesLayer = (GridValueLayer)context.getValueLayer("Species");
		
		resourceLayer.set(resource, grid.getLocation(this).getX(), grid.getLocation(this).getY());
		speciesLayer.set(species, grid.getLocation(this).getX(), grid.getLocation(this).getY());
	}
	
	public double getResource() {
		return resource;
	}

	public void setResource(double resource) {
		this.resource = resource;
	}

	public double getSpecies() {
		return species;
	}

	public void setSpecies(double species) {
		this.species = species;
	}
}