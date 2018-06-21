package sir;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.query.space.grid.MooreQuery;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.StrictBorders;
import repast.simphony.util.ContextUtils;

public class SIRBuilder implements ContextBuilder<ISIRAgent> {

	public final static String CONTEXT_ID = "SIR";
	
	@Override
	public Context<ISIRAgent> build(Context<ISIRAgent> context) {
		context.setId(CONTEXT_ID);
		
		Parameters params = RunEnvironment.getInstance().getParameters();
		int gridWidth = (Integer) params.getValue("gridWidth");
		int gridHeight = (Integer) params.getValue("gridHeight");
		double illnessDuration = (Double) params.getValue("illness_duration");
		int contacts = (Integer) params.getValue("contacts");
		double infectionProb = (Double) params.getValue("infection_probability");
		
		List<ISIRAgent> agents = new ArrayList<ISIRAgent>();
		
		Grid<ISIRAgent> grid = GridFactoryFinder.createGridFactory(null).createGrid("Grid",
			context, GridBuilderParameters.singleOccupancy2D(new RandomGridAdder<ISIRAgent>(),
							new StrictBorders(), gridWidth, gridHeight));

		for (int x = 0; x < gridWidth; ++x) {
			for (int y = 0; y < gridHeight; ++y) {
				boolean infected = false;
				ISIRAgent a = null;
				
				if (x == gridWidth / 2 && y == gridHeight / 2)
					infected = true;
				
				a = new SIRStateChartAgent(infected, contacts, infectionProb, illnessDuration);
				
				context.add( a );
				agents.add( a );
				
				grid.moveTo(a, x, y);
			}
		}

		return context;
	}
	
	@SuppressWarnings("unchecked")
	public static ISIRAgent getRandomNeighbour(ISIRAgent self) {
		Context<ISIRAgent> context = (Context<ISIRAgent>) ContextUtils.getContext(self);
		Grid<ISIRAgent> grid = (Grid<ISIRAgent>)context.getProjection("Grid");
		MooreQuery<ISIRAgent> moore = new MooreQuery<ISIRAgent>(grid, self, 1, 1);
		
		List<ISIRAgent> neighbours = new ArrayList<ISIRAgent>();
		Iterator<ISIRAgent> iter = moore.query().iterator();
		
		while (iter.hasNext()) {
			neighbours.add(iter.next());
		}
		
		int randIdx = (int) (Math.random() * neighbours.size());
		return neighbours.get(randIdx);
	}
}
