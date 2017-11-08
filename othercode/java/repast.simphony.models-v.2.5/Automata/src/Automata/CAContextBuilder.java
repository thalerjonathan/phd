package Automata;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;
import repast.simphony.valueLayer.GridValueLayer;

/**
 * Cellular automata with parity rule.
 * 
 * @author Eric Tatara
 *
 */
public class CAContextBuilder implements ContextBuilder<CA> {

	public Context<CA> build(Context<CA> context) {

		Parameters parm = RunEnvironment.getInstance().getParameters();
		int gridWidth = (Integer)parm.getValue("gridWidth");
		int gridHeight = (Integer)parm.getValue("gridHeight");
		int seedWidth = (Integer)parm.getValue("centerSeedWidth");

		// Create the grid for the CAs
		Grid<CA> grid = GridFactoryFinder.createGridFactory(null).createGrid("Grid", context, 
				new GridBuilderParameters<CA>(new WrapAroundBorders(), 
						new RandomGridAdder<CA>(), false, gridWidth, gridHeight));

		// Create a value layer to store the state for each CA
		GridValueLayer valueLayer = new GridValueLayer("State",true,
				new WrapAroundBorders(), gridWidth, gridHeight);

		// Add the value layers to the context 
		context.addValueLayer(valueLayer);

		// Create and place a new CA in each grid location.
		for (int j=0; j<gridHeight; j++){
			for (int i=0; i<gridWidth; i++){
				CA ca = new CA();
				context.add(ca);
				grid.moveTo(ca, i, j);
			}
		}

		// "Seed" the the center CA.
		for (int j=gridHeight/2-seedWidth/2; j<gridHeight/2+seedWidth/2; j++){
			for (int i=gridWidth/2-seedWidth/2; i<gridWidth/2+seedWidth/2; i++){
				ScheduleParameters scheduleParams = ScheduleParameters.createOneTime(0);
				CA ca = grid.getObjectAt(i,j);
				RunEnvironment.getInstance().getCurrentSchedule().schedule(scheduleParams, 
						ca, "setState", 1);
			}
		}
		
		return context;
	}
}