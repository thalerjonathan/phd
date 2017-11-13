package socialForce;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactory;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.SimpleCartesianAdder;
import repast.simphony.space.continuous.StickyBorders;
import repast.simphony.space.continuous.StrictBorders;

public class SocialForceBuilder implements ContextBuilder<Object> {

	public final static String CONTEXT_ID = "SocialForce";
	public final static String SPACE_ID = "SocialForceSpace";
	
	private final static double SPACE_WIDTH = 1000;
	private final static double SPACE_HEIGHT = 600;
	
	@Override
	public Context<Object> build(Context<Object> context) {
		context.setId(CONTEXT_ID);

		ContinuousSpaceFactory spaceFactory = ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null);

		double[] dimensions = { SPACE_WIDTH, SPACE_HEIGHT };
		double[] origin = { 0, 0 };
		
		ContinuousSpace<Object> space = 
				spaceFactory.createContinuousSpace(
						SPACE_ID, 
						context,
						new SimpleCartesianAdder<Object>(),
						new StickyBorders(), //StrictBorders(), // StickyBorders()
						dimensions,
						origin );

		SocialForce main = new SocialForce(space);
		main.initAgents(context);
		
		context.add(main);
		
		return context;
	}
}
