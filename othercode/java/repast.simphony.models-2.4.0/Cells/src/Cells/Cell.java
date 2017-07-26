package Cells;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

/**
 * Implementation of a bovine pulmonary artery endothelial (BPAE) cell with 
 * contact-inhibited migration as described in
 * 
 *     Lee, Y., S. Kouvroukoglou, L. McIntire, and K. Zygourakis, "A Cellular
 *         Automata Model for the Proliferation of Migrating Contact-Inhibited 
 *         Cells," Biophysics Journal, 69, 1284-1298, 1995.
 * 
 * The cells have a random walk across a 2D Grid and divide according to walk
 * and cell phase parameters. 
 * 
 * @author Eric Tatara
 *
 */
public class Cell {

	/**
	 * The cell movement counter.  When this counter reaches 0, the cell may move
	 *  to an adjacent site.
	 */
	private int moveCounter;
	
	/**
	 * The cell reproduction counter.  When this counter reaches 0, the cell may
	 *  divide if an adjacent site is available.
	 */
	private int reproduceCounter;
	
	/**
	 * The time to move, set initially.
	 */
	private int divideTime;
	
	/**
	 * The time to divide, set initially.
	 */
	private int moveTime;
	
	public Cell(){
		Parameters p = RunEnvironment.getInstance().getParameters();
		
		int divideTimeMin = (Integer)p.getValue("divideTimeMin");
		int divideTimeMax = (Integer)p.getValue("divideTimeMax");
		int moveTimeMin = (Integer)p.getValue("moveTimeMin");
		int moveTimeMax = (Integer)p.getValue("moveTimeMax");
		
		// Set the initial move and divide times according to the specified parameters
		// and sampled from a Uniform distribution.
		divideTime = RandomHelper.nextIntFromTo(divideTimeMin, divideTimeMax);
		moveTime = RandomHelper.nextIntFromTo(moveTimeMin, moveTimeMax);
		
		reproduceCounter = divideTime;;
		moveCounter = moveTime;
	}
	
	@ScheduledMethod(start=1, interval=1)
	public void step(){
	  // If the the move counter reaches 0
		if (moveCounter == 0){
			// and if there is an empty adjacent site
			if (!findEmptySites().isEmpty())
				// move to a random empty adjacent site
				move();
			 
			// reset the move counter
			moveCounter = moveTime;
		}
		else // continue waiting to move
			moveCounter--;
		
		// if the reproduce counter reaches 0
		if (reproduceCounter == 0){
			// and if there is an empty adjacent site
			if (!findEmptySites().isEmpty())
				reproduce();
			 
			// reset the reproduce counter
			reproduceCounter = divideTime;
		}
		else // continue waiting to reproduce
			reproduceCounter--;
	
	}
	
	/**
	 * Move the cell to a random empty adjacent site.
	 */
	private void move(){
		Context context = ContextUtils.getContext(this);
		Grid grid = (Grid)context.getProjection("Grid");
	
		List<GridPoint> emptySites = findEmptySites();
		
		// TODO add Grid.moveTo(Object o, GridPoint pt) to Repast API
		if (emptySites.size() > 0) grid.moveTo(this, emptySites.get(0).getX(), emptySites.get(0).getY());
	}
	
	/**
	 * Produce a daughter cell and move it to an empty adjacent site.
	 */
	private void reproduce(){		
		Context context = ContextUtils.getContext(this);
		Grid grid = (Grid)context.getProjection("Grid");
	
		Cell cell = new Cell();	
		context.add(cell);
		
		List<GridPoint> emptySites = findEmptySites();
		
		// TODO add Grid.moveTo(Object o, GridPoint pt) to Repast API
		if (emptySites.size() > 0) grid.moveTo(cell, emptySites.get(0).getX(), emptySites.get(0).getY());
		
	}
	
	/**
	 * Provides a list of adjacent (unoccupied) sites in the cell's Moore 
	 * neighborhood.  The list of sites is shuffled.
	 * 
	 * @return the list of adjacent sites.
	 */
	private List<GridPoint> findEmptySites(){
		List<GridPoint> emptySites = new ArrayList<GridPoint>();
		Context context = ContextUtils.getContext(this);
		Grid grid = (Grid)context.getProjection("Grid");
		GridPoint pt = grid.getLocation(this);
		
		// Find Empty Moore neighbors
		// TODO automate via Repast API
		if (!grid.getObjectsAt(pt.getX()-1,pt.getY()+1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()-1,pt.getY()+1));
		if (!grid.getObjectsAt(pt.getX(),pt.getY()+1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX(),pt.getY()+1));
		if (!grid.getObjectsAt(pt.getX()+1,pt.getY()+1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()+1,pt.getY()+1));
		if (!grid.getObjectsAt(pt.getX()+1,pt.getY()).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()+1,pt.getY()));
		if (!grid.getObjectsAt(pt.getX()+1,pt.getY()-1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()+1,pt.getY()-1));
		if (!grid.getObjectsAt(pt.getX(),pt.getY()-1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX(),pt.getY()-1));
		if (!grid.getObjectsAt(pt.getX()-1,pt.getY()-1).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()-1,pt.getY()-1));
		if (!grid.getObjectsAt(pt.getX()-1,pt.getY()).iterator().hasNext())
			emptySites.add(new GridPoint(pt.getX()-1,pt.getY()));
		
		Collections.shuffle(emptySites);
		
		return emptySites;
	}
}
