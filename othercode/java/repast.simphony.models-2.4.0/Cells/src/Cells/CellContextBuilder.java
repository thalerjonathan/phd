package Cells;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;

/**
 * Implementation of a bovine pulmonary artery endothelial (BPAE) cell model 
 * with contact-inhibited migration as described in
 * 
 *     Lee, Y., S. Kouvroukoglou, L. McIntire, and K. Zygourakis, "A Cellular
 *         Automata Model for the Proliferation of Migrating Contact-Inhibited 
 *         Cells," Biophysics Journal, 69, 1284-1298, 1995.
 * 
 * @author Eric Tatara
 *
 */
public class CellContextBuilder implements ContextBuilder {

	@Override
	public Context build(Context context) {
		Parameters p = RunEnvironment.getInstance().getParameters();
		
		int gridWidth = (Integer)p.getValue("gridWidth");
		int gridHeight = (Integer)p.getValue("gridHeight");
		int initialCells = (Integer)p.getValue("initialCells");
		
		GridFactoryFinder.createGridFactory(null).createGrid("Grid",
				context, GridBuilderParameters.singleOccupancy2D(new RandomGridAdder(),
								new WrapAroundBorders(), gridWidth, gridHeight));
		
		for (int i=0; i<initialCells; i++){
			context.add(new Cell());
		}
		
		return context;
	}

}
