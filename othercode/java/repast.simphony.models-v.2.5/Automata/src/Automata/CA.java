package Automata;

import java.util.Iterator;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.query.space.grid.VNQuery;
import repast.simphony.space.grid.Grid;
import repast.simphony.util.ContextUtils;
import repast.simphony.valueLayer.GridValueLayer;

/**
 * A cellular automata that follows the parity rule described in 
 * 
 *   Chopard, B. and M. Droz, Cellular Automata Modeling of Physical Systems,
 *        Cambridge University Press, Cambridge, UK, 1998.
 *        
 * Each CA has a 1-bit state (0 or 1) which is calculated from the states of
 * its neighbors at the previous time step such that the state is 0 if the sum
 * if the surrounding cell states is even, otherwise the state is 1.  
 * 
 * Mathematically, the state of a CA at position i,j can be expressed using the 
 * exclusive or operator (XOR) ("^" in Java):
 * 
 *     Y(i,j) = Y(i+1,j) XOR Y(i-1,j) XOR Y(i,j+1) XOR Y(i,j-1)
 * 
 * @author Eric Tatara
 *
 */
public class CA {
	
	public int state;
	public int oldState;

	/**
	 * Maintain the history of the state for one time interval
	 */
	@ScheduledMethod(start=1, interval=1, priority=ScheduleParameters.FIRST_PRIORITY)
	public void setOldState(){
		oldState = state;
	}
	
	@ScheduledMethod(start=1, interval=1)
	public void step(){
		Context<CA> context = ContextUtils.getContext(this);
		Grid<CA> grid = (Grid)context.getProjection("Grid");
		
		// Query the von Neuman neighborhood
		VNQuery<CA> query = new VNQuery(grid,this);
		Iterator<CA> iter = query.query().iterator();
				
		// Get the states of the four neighbors;
		int a =	iter.next().getOldState();
		int b =	iter.next().getOldState();
		int c =	iter.next().getOldState();
		int d =	iter.next().getOldState();
		
		// set the state according to the parity rule.
		setState(a^b^c^d);
	}
	
	public int getState() {
		return state;
	}	
	public int getOldState() {
		return oldState;
	}
	public void setOldState(int oldState) {
		this.oldState = oldState;
	}

	public void setState(int state) {
		this.state = state;
		
		// also store the state in the value layer for animation
		Context<CA> context = ContextUtils.getContext(this);
		Grid<CA> grid = (Grid)context.getProjection("Grid");
    GridValueLayer vl = (GridValueLayer)context.getValueLayer("State");
    vl.set(state, grid.getLocation(this).getX(), grid.getLocation(this).getY());
	}
}