package SugarScape;

import repast.simphony.context.DefaultContext;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;
import repast.simphony.valueLayer.GridValueLayer;

/**
 * The space in which the SugarAgents act. In particular this space implements
 * the growback rule G from Growing Artificial Societies.
 *
 * The source has been annotated so see that for further details.
 *
 * @author Eric Tatara
 * @author Nick Collier
 * @version 
 */

public class SugarSpace extends DefaultContext<Object> {

	// the Sugar space has a grid that tracks the maximum
	// amount of sugar and the current value at each x,y coordinate.

	// default sugar grow rate and the default maximum sugar
	Parameters p = RunEnvironment.getInstance().getParameters();

	int sugarGrowRate = (Integer)p.getValue("sugarGrowRate");

	private int xdim = (Integer)p.getValue("worldWidth");
	private int ydim = (Integer)p.getValue("worldHeight");

	public SugarSpace(String sugarFile) {
		super("SugarSpace");

		Grid<Object> grid = GridFactoryFinder.createGridFactory(null)
		.createGrid("Grid", this, new GridBuilderParameters<Object>(
				new WrapAroundBorders(), new RandomGridAdder<Object>(), true, xdim, ydim));

		GridValueLayer currentSugar = new GridValueLayer("CurrentSugar",true,
				new WrapAroundBorders(), xdim, ydim);
		GridValueLayer maxSugar = new GridValueLayer("MaxSugar", true, 
				new WrapAroundBorders(), xdim, ydim);
		this.addValueLayer(currentSugar);
		this.addValueLayer(maxSugar);

		// Read in the sugar values from a file
		PGMReader reader = new PGMReader(sugarFile);
		int matrix[][] = reader.getMatrix();

		// create the sugar and fill the sugar space
		for (int x = 0; x < xdim; x++) {
			for (int y = 0; y < ydim; y++) {    	
				currentSugar.set(matrix[x][y], x,y);
				maxSugar.set(matrix[x][y], x,y);
			}
		}
	}

	// The actual implementation of growback rule G, pg 182 (Appendix B).
	@ScheduledMethod(start=0,interval=1)
	public void updateSugar() {
		int sugarAtSpot;
		int maxSugarAtSpot;

		GridValueLayer currentSugar = (GridValueLayer)getValueLayer("CurrentSugar");
		GridValueLayer maxSugar = (GridValueLayer)getValueLayer("MaxSugar");

		for (int i = 0; i < xdim; i++) {
			for (int j = 0; j < ydim; j++) {
				sugarAtSpot = (int)currentSugar.get(i,j);
				maxSugarAtSpot = (int)maxSugar.get(i,j);

				if (sugarGrowRate == -1) 
					currentSugar.set(maxSugarAtSpot,i,j);

				else 
					if (sugarAtSpot != maxSugarAtSpot) 
						if (sugarAtSpot + sugarGrowRate <= maxSugarAtSpot) 
							currentSugar.set(sugarAtSpot + sugarGrowRate, i, j);

						else 
							currentSugar.set(maxSugarAtSpot, i, j);         
			}
		}
	}

	// takes all the sugar at this coordinate, leaving no sugar.
	public int takeSugarAt(int x, int y) {
		GridValueLayer currentSugar = (GridValueLayer)getValueLayer("CurrentSugar");
		int i = (int)currentSugar.get(x,y);		
		currentSugar.set(0, x,y);
		
		return i;
	}

	// gets the amount of sugar at this x,y coordinate
	public int getSugarAt(int x, int y) {		
		return (int)getValueLayer("CurrentSugar").get(x,y);
	}
}