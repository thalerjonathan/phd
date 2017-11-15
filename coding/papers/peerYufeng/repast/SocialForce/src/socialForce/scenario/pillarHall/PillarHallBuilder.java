package socialForce.scenario.pillarHall;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactory;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.SimpleCartesianAdder;
import repast.simphony.space.continuous.StrictBorders;

public class PillarHallBuilder implements ContextBuilder<Object> {

	public final static String CONTEXT_ID = "PillarHall";
	public final static String SPACE_ID = "PillarHallSpace";
	
	public final static double SPACE_WIDTH = 1000;
	public final static double SPACE_HEIGHT = 600;
	
	@Override
	public Context<Object> build(Context<Object> context) {
		context.setId(CONTEXT_ID);

		ContinuousSpaceFactory spaceFactory = ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null);

		double[] dimensions = { SPACE_WIDTH, SPACE_HEIGHT };
		
		ContinuousSpace<Object> space = 
				spaceFactory.createContinuousSpace(
						SPACE_ID, 
						context,
						new SimpleCartesianAdder<Object>(),
						new StrictBorders(), //StickyBorders()
						dimensions );

		PillarHall main = new PillarHall(space);
		main.initAgents(context);
		
		context.add(main);
		space.moveTo(main, 0, 0);
		
		return context;
	}
}
